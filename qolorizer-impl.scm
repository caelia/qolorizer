(import data-structures)
(import extras)
(use cairo)

(define (check1 x)
  (or (and (>= x 0) (<= x 1))
      (error (sprintf "Invalid input: ~A\n" x))))

(define (check255 x)
  (or (and (>= x 0) (<= x 255))
      (error (sprintf "Invalid input: ~A\n" x))))

(define (check-component lower upper)
  (lambda (c #!optional tag)
    (let ((tagstr (if tag (string-append "[" tag "] ") "")))
      (unless (and (>= c lower) (<= c upper))
        (printf "~AValue out of range: ~A\n" tagstr c)))))

(define (check255/2 a b)
  (or (and (>= a 0)
           (<= a 255)
           (>= b 0)
           (<= b 255))
      (error (sprintf "Invalid input: ~A, ~A\n" a b))))

(define (check-hsv h s v)
  (or (and (>= h 0) (<= h 360) (>= s 0) (<= s 1) (>= v 0) (<= v 1))
      (error (sprintf "Invalid input| h: ~A, s: ~A, v: ~A\n" h s v))))

(define (check-hsv/2 h0 s0 v0 h1 s1 v1)
  (or (and (>= h0 0)
           (<= h0 360)
           (>= h1 0)
           (<= h1 360)
           (>= s0 0)
           (<= s0 1)
           (>= v0 0)
           (<= v0 1)
           (>= s1 0)
           (<= s1 1)
           (>= v1 0)
           (<= v1 1))
      (error
        (sprintf
          "Invalid input| h0: ~A, s0: ~A, v0: ~A, h1: ~A, s1: ~A, v1: ~A\n"
          h0 s0 v0 h1 s1 v1))))

(define (x>int x)
  (inexact->exact (round x)))

(define (x>int255 x)
  (inexact->exact (round (* x 255))))

(define (clamp1 x)
  (min (max x 0) 1))

(define (clamp255 x)
  (min (max x 0) 255))


(define (parse-color spec alpha)
  (let ((s>n
          (lambda (s) (/ (string->number (string-append "#x" s)) 255)))
        (c>s
          (lambda (c) (list->string (list c c))))
        (badspec
          (lambda () (error (sprintf "Invalid color spec: '~A'" spec))))
        (verify
           (lambda (x n)
             (if (and (number? x) (>= x 0) (<= x n))
               x
               (error (sprintf "Invalid color value: '~A'" x))))))
    (cond
      ((list? spec)
        (let ((r (verify (car spec) 255))
              (g (verify (cadr spec) 255))
              (b (verify (caddr spec) 255))
              (a (verify
                   (or alpha
                     (and (= (length spec) 4) (cadddr spec))
                     1.0)
                   1)))
          (values (/ r 255) (/ g 255) (/ b 255) a)))
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
            (values (s>n r*) (s>n g*) (s>n b*)
                    (or (and a* (s>n a*))
                        (and alpha (verify alpha 1))
                        1.0)))))
      ((string? spec)
       (let* ((parts (map string->number (string-split spec ",")))
              (len (length parts)))
         (if (or (= len 3) (= len 4))
           (parse-color parts alpha)
           (badspec))))
      (else
        (badspec)))))

(define (get-cairo-blend-mode mode)
  (alist-ref
    mode
    `((normal . ,CAIRO_OPERATOR_OVER) (multiply . ,CAIRO_OPERATOR_MULTIPLY)
      (screen . ,CAIRO_OPERATOR_SCREEN) (overlay . ,CAIRO_OPERATOR_OVERLAY)
      (darken-only . ,CAIRO_OPERATOR_DARKEN) (lighten-only . ,CAIRO_OPERATOR_LIGHTEN)
      (dodge . ,CAIRO_OPERATOR_COLOR_DODGE) (burn . ,CAIRO_OPERATOR_COLOR_BURN)
      (hard-light . ,CAIRO_OPERATOR_HARD_LIGHT) (soft-light . ,CAIRO_OPERATOR_SOFT_LIGHT)
      (difference . ,CAIRO_OPERATOR_DIFFERENCE) (exclusion . ,CAIRO_OPERATOR_EXCLUSION)
      (hue . ,CAIRO_OPERATOR_HSL_HUE) (saturation . ,CAIRO_OPERATOR_HSL_SATURATION)
      (color . ,CAIRO_OPERATOR_HSL_COLOR) (luminosity . ,CAIRO_OPERATOR_HSL_LUMINOSITY))))
                    
(define (mk-blend-op color-spec #!key (blend-mode 'normal) (alpha #f))
  (let-values (((r g b a) (parse-color color-spec alpha)))
    (let ((mode-spec (get-cairo-blend-mode blend-mode)))
      (lambda (src-file dest-file)
        (let* ((isurf (cairo-image-surface-create-from-png src-file))
               (width (cairo-image-surface-get-width isurf)) 
               (height (cairo-image-surface-get-height isurf)) 
               (surf (cairo-image-surface-create CAIRO_FORMAT_ARGB32 width height)) 
               (ctx (cairo-create surf))
               (ictx (cairo-create isurf)))
          (cairo-set-source-rgba ctx r g b a)
          (cairo-rectangle ctx 0 0 width height)
          (cairo-fill ctx)
          (cairo-set-operator ctx mode-spec)
          (cairo-set-source-surface ctx isurf 0 0)
          (cairo-paint ctx)
          (cairo-set-operator ictx CAIRO_OPERATOR_IN)
          (cairo-set-source-surface ictx surf 0 0)
          (cairo-paint ictx)
          (cairo-surface-write-to-png isurf dest-file)
          (cairo-surface-destroy isurf)
          (cairo-surface-destroy surf))))))

(define (colorize blend-op src-file dest-file)
  (blend-op src-file dest-file))
