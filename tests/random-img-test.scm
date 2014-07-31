(use imlib2)

(define max-width 800)
(define min-width 200)
(define max-height 600)
(define min-height 150)

(define (get-img-dimensions)
  (values
    (+ (random (- max-width min-width)) min-width)
    (+ (random (- max-height min-height)) min-height)))

(define (random-pixel)
  (list (random 255) (random 255) (random 255) (random 255)))

(define (get-pixels w h)
  (let loop ((x 0) (y 0) (rows '()) (row-pixels '()))
    (cond
      ((>= y h) rows)
      ((>= x w) (loop 0 (+ y 1) (cons row-pixels rows) '()))
      (else (loop (+ x 1) y rows (cons (random-pixel) row-pixels))))))

(define (create-image pixels)
  (let* ((width (length (car pixels)))
         (height (length pixels))
         (img (image-create width height)))
    (let loop ((x 0) (y 0))
      (cond
        ((>= y height) img)
        ((>= x width) (loop 0 (+ y 1)))
        (else
          (let* ((px (list-ref (list-ref pixels y) x))
                 (r (car px))
                 (g (cadr px))
                 (b (caddr px))
                 (a (cadddr px)))
            (image-draw-pixel img (color/rgba r g b a) x y)
            (loop (+ x 1) y)))))))

(define (pixel-discrepancy? img pxx x y)
  (let-values (((ir ig ib ia) (image-pixel/rgba img x y)))
    (let* ((dpx (list-ref (list-ref pxx y) x))
           (dr (car dpx))
           (dg (cadr dpx))
           (db (caddr dpx))
           (da (cadddr dpx)))
      (if (or (and (= ia 0) (= da 0))
              (and (= ir dr) (= ig dg) (= ib db) (= ia da)))
        #f
        (sprintf "(~A, ~A): Expected ~A/~A/~A/~A but got ~A/~A/~A/~A" x y dr dg db da ir ig ib ia)))))

(define (verify-image file pixels #!key (stop-on-error #f) (destroy #t))
  (let* ((img (image-load file))
         (width (image-width img))
         (height (image-height img))
         (pw (length (car pixels)))
         (ph (length pixels)))
    (unless (and (= height ph) (= width pw))
      (error (sprintf "Size mismatch: expected ~Ax~A but got ~Ax~A" pw ph width height)))
    (let ((done (lambda (value) (when destroy (image-destroy img)) value)))
      (let loop ((x 0) (y 0) (have-errors #f))
        (cond
          ((>= y height) (done (not have-errors)))
          ((>= x width) (loop 0 (+ y 1) have-errors))
          (else
            (let ((disc (pixel-discrepancy? img pixels x y)))
              (if disc
                (begin
                  (print disc)
                  (if stop-on-error
                    (done #f)
                    (loop (+ x 1) y #t)))
                (loop (+ x 1) y have-errors)))))))))

(define (create-and-test filename)
  (let-values (((w h) (get-img-dimensions)))
    (let* ((pixels (get-pixels w h))
           (img (create-image pixels)))
      (image-save img filename)
      (image-destroy img)
      (when (verify-image filename pixels)
        (print "No Errors.")))))

(define (create-and-save img-file pix-file)
  (let-values (((w h) (get-img-dimensions)))
    (let* ((pixels (get-pixels w h))
           (img (create-image pixels)))
      (image-save img img-file)
      (image-destroy img)
      (with-output-to-file
        pix-file
        (lambda () (write pixels))))))

(define (load-and-test img-file pix-file)
  (let ((pixels (with-input-from-file pix-file read)))
    (when (verify-image img-file pixels)
      (print "No Errors."))))
