(module colorizer
        colorize
        
        (import scheme chicken)
        (import data-structures)
        (use imlib2)

(define (parse-color spec alpha)
  (let ((@> (lambda (n) (/ n 255)))
        (s>n (lambda (s) (string->number (string-append "#" s))))
        (c>s (lambda (c) (list->string (list c c))))
        (badspec (lambda () (error (sprintf "Invalid color spec: '~A'" spec)))))
    (cond
      ((list? spec)
        (let ((a (or alpha
                     (and (= (length spec) 4)
                          (cadddr spec))
                     1.0)))
          (values (@> (car spec)) (@> (cadr spec)) (@> (caddr spec)) a)))
      ((and (string? spec) (eqv? (string-ref spec 0) #\#))
        (let ((len (string-length spec)))
          (let-values (((r* g* b*)
                        (cond
                          ((or (= len 4) (= len 5))
                            (values (c>s (string-ref spec 1))
                                    (c>s (string-ref spec 2))
                                    (c>s (string-ref spec 3))))
                          ((or (= len 7) (= len 9))
                            (values (substring spec 1 3)
                                    (substring spec 3 5)
                                    (substring spec 5 7)))
                          (else (badspec))))
                        ((a*)
                          (and (not alpha)
                               (or (and (= len 5) (c>s (string-ref spec 4)))
                                   (and (= len 9) (substring spec 7 9))))))
            (values (@> (s>n r*)) (@> (s>n b*)) (@> (s>n g*))
                    (or (and a* (@> (s>n a*)))
                        alpha
                        1.0)))))
      ((string? spec)
       (let* ((parts (map string->number (string-split spec ",")))
              (len (length parts)))
         (if (or (= len 3) (= len 4))
           (parse-color parts alpha)
           (badspec))))
      (else
        (badspec)))))

; (define (blend- r0 g0 b0 a0 r1 g1 b1 a1)

(define (blend-normal r0 g0 b0 a0 r1 g1 b1 a1) 
  (cond
    ((= a1 1) (values r1 g1 b1 1.0))
    ((= a1 0) (values r0 g0 b0 0.0))))

(define (blend-multiply r0 g0 b0 a0 r1 g1 b1)
  (values (* r0 r1) (* g0 g1) (* b0 b1) (* a0 a1)))

(define (blend-screen r0 g0 b0 a0 r1 g1 b1 a1)
  (let ((f (lambda (v0 v1) (

(define (calculate-color r0 g0 b0 a0 r1 g1 b1 a1 mode)
  (let ((op
          (case mode
            ((normal) blend-normal)
            ((multiply) blend-multiply)
            ((screen) blend-screen)
            ((dodge) blend-dodge)
            ((burn) blend-burn))
            (else (error (sprintf "Unknown blend mode: '~A'" mode))))
         (@> (lambda (n) (/ n 255)))
         (<@ (lambda (n) (inexact->exact (round (* n 255))))))
    (let-values (((r* g* b* a*)
                  (op (@> r0) (@> g0) (@> b0) (@> a0) r1 g1 b1 a1)))
      (color/rgba (<@ r*) (<@ g*) (<@ b*) (<@ a*)))))

(define (colorize src-img color-spec #!key (blend-mode 'normal) (alpha #f))
  (let* ((width (image-width src-img))
         (height (image-height src-img))
         (dest (image-create width height)))
    (let-values (((r1 g1 b1 a1) (parse-color color-spec alpha)))
      (let hloop ((x 0))
        (when (< x width)
          (let vloop ((y 0))
            (when (< y height)
              (let-values (((r0 g0 b0 a0) (image-pixel/rgba src-img x y)))
                (let ((final-color
                        (calculate-color r0 g0 b0 a0 r1 g1 b1 a1 blend-mode)))
                  (image-draw-pixel dest final-color)))
              (vloop (+ y 1))))
          (hloop (+ x 1)))))
    dest))
