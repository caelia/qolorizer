(import data-structures)
(import extras)
(use imlib2)

(define (check255 x)
  (or (and (>= x 0) (<= x 255))
      (error (sprintf "Invalid input: ~A\n" x))))

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

(define (clamp255 x)
  (min (max x 0) 255))

;; Got these formulae from http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
;; I'm not positive they're correct. Alternative sources include:
;;   http://www.cs.rit.edu/~ncs/color/t_convert.html
;;   http://www.easyrgb.com/index.php?X=MATH&H=21#text21
;;   http://stackoverflow.com/questions/3018313/\
;;     algorithm-to-convert-rgb-to-hsv-and-hsv-to-rgb-in-range-0-255-for-both
(define (hsv>rgb h s v)
  (let ((@> (lambda (x) (x>int (* x 255)))))
    (if (= s 0)
      (values (@> v) (@> v) (@> v))
      (let* ((c (* v s))
             (x (* c (- 1 (abs (- (modulo (/ h 60) 2) 1)))))
             (m (- v c)))
        (let-values (((r* g* b*)
                      (cond
                        ((>= h 360) (error (sprintf "Hue > 360? [~A]" h)))
                        ((< h 0) (error (sprintf "Hue < 0? [~A]" h)))
                        ((< h 60) (values c x 0))
                        ((< h 120) (values x c 0))
                        ((< h 180) (values 0 c x))
                        ((< h 240) (values 0 x c))
                        ((< h 300) (values x 0 c))
                        (else (values c 0 x)))))
          (values (@> (+ r* m)) (@> (+ g* m)) (@> (+ b* m))))))))

(define (rgb>hsv r g b)
  (let* ((r* (/ r 255))
         (g* (/ g 255))
         (b* (/ b 255))
         (cmax (max r* g* b*))
         (cmin (min r* g* b*))
         (delta (- cmax cmin)))
    (let ((h
           (*
             60
             (cond
               ((= cmax r*) (modulo (/ (- g* b*) delta) 6))
               ((= cmax g*) (+ (/ (- b* r*) delta) 2))
               (else (+ (/ (- r* g*) delta) 4)))))
          (s
            (if (= delta 0)
              0
              (/ delta cmax)))
          (v cmax))
      (values h s v))))

(define (parse-color spec alpha)
  (let ((s>n
          (lambda (s) (string->number (string-append "#x" s))))
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
          (values r g b a)))
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
                    (or (and a* (/ (s>n a*) 255))
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

(define (normal-op m)
  (check255 m) 
  (lambda (i) (check255 i) m))

(define (dissolve-op m)
  (check255 m) 
  (lambda (i) (check255 i) (if (= (random 2) 0) i m)))

(define (multiply-op m)
  (check255 m) 
  (lambda (i) (check255 i) (x>int (/ (* i m) 255))))

(define (screen-op m)
  (check255 m) 
  (let ((a (- 255 m)))
    (lambda (i)
      (check255 i)
      (x>int (- 255 (/ (* a (- 255 i)) 255))))))

(define (overlay-op m)
  (check255 m) 
  (let ((a (* 2 m)))
    (lambda (i)
      (check255 i)
      (x>int (* (/ i 255) (+ i (* (/ a 255) (- 255 i))))))))

(define (hard-light-op m)
  (check255 m) 
  (let ((a (* 2 (- m 128))))
    (lambda (i)
    (check255 i)
    (x>int
      (if (> m 128)
        (- 255 (/ (* (- 255 a) (- 255 i)) 256))
        (/ (* 2 m i) 256))))))

;; This formula seems completely broken
; (define (soft-light i m)
  ; (check255/2 i m)
  ; (x>int (* (/ (+ (* (- 255 i) m) (screen i m)) 255) i)))

;; Here is the W3C formula
(define (soft-light-op m)
  (check255 m) 
  (let* ((m* (/ m 255))
         (a (- (* 2 m*) 1))
         (b (- 1 (* 2 m*))))
    (lambda (i)
      (check255 i)
      (let ((i* (/ i 255)))
        (let ((res
                (cond
                  ((and (> m* 0.5) (> i* 0.25))
                   (+ i* (* a (- (sqrt i*) i*))))
                  ((> m* 0.5)
                   (+ i* (* a (- (* (+ (* (- (* 16 i*) 12) i*) 4) i*) i*))))
                  (else
                    (- i* (* b i* (- 1 i*)))))))
          (x>int (* 255 res)))))))

;; ???
(define (dodge-op m)
  (check255 m) 
  (let ((a (+ (- 255 m) 1)))
    (lambda (i)
      (check255 i)
      (clamp255 (x>int (/ (* 256 i) a))))))

;; ???
(define (burn-op m)
  (check255 m) 
  (let ((a (+ m 1)))
    (lambda (i)
      (check255 i)
      (clamp255 (x>int (- 255 (/ (* 256 (- 255 i)) a)))))))

;; 1. Based on test results, the formula in the GIMP manual matches
;;    program behavior, except values are constrained to 0 <= x <= 255.
(define (divide-op m)
  (check255 m) 
  (let ((a (+ m 1)))
    (lambda (i)
      (check255 i)
      (clamp255 (x>int (/ (* 256 i) a))))))

(define (difference-op m)
  (check255 m) 
  (lambda (i) (check255 i) (abs (- i m))))

(define (addition-op m)
  (check255 m) 
  (lambda (i) (check255 i) (min (+ i m) 255)))

(define (subtract-op m)
  (check255 m) 
  (lambda (i) (check255 i) (max (- i m) 0)))

; (define darken-only min)

; (define lighten-only max)

;; Temporary defs for debugging
(define (darken-only-op m)
  (check255 m) 
  (lambda (i) (check255 i) (min i m)))

(define (lighten-only-op m)
  (check255 m) 
  (lambda (i) (check255 i) (max i m)))

(define (grain-extract-op m)
  (check255 m) 
  (lambda (i)
    (check255 i)
    (max (min (+ (- i m) 128) 255) 0)))

(define (grain-merge-op m)
  (check255 m) 
  (lambda (i)
    (check255 i)
    (max (min (- (+ i m) 128) 255) 0)))

(define (color-op hm sm vm)
  (check-hsv hm sm vm)
  (lambda (hi si vi) (check-hsv hi si vi) (values hm sm vi)))
  
(define (hue-op hm sm vm)
  (check-hsv hm sm vm)
  (let ((hop
         (lambda (hi si vi)
           (if (= sm 0)
             (values hi si vi)
             (values hm si vi)))))
    (lambda (hi si vi) (check-hsv hi si vi) (hop hi si vi))))
  
(define (saturation-op hm sm vm)
  (check-hsv hm sm vm)
  (lambda (hi si vi) (check-hsv hi si vi) (values hi sm vi)))

(define (value-op hm sm vm)
  (check-hsv hm sm vm)
  (lambda (hi si vi) (check-hsv hi si vi) (values hi si vm)))


(define (source-over ri gi bi ai rm gm bm am)
  ; (let* ((a (- 1 (* (- 1 am) (- 1 ai)))) ; don't know where I got this
  (let* ((a (- (+ ai am) (/ ai am)))
         (f
           (lambda (ci cm)
             (/ (+ (* cm am) (* ci ai (- 1 am))) a))))
    (values (f ri rm) (f gi gm) (f bi bm) a)))

(define (composite-op am #!optional (method 'source-over))
  (let ((op
         (case method
           ((source-over) source-over)
           (else (error (sprintf "Unsupported compositing method: '~A'" method))))))
    (lambda (ri gi bi ai rm gm bm)
      (let-values (((r g b a) (op ri gi bi ai rm gm bm am)))
        (color/rgba (x>int r) (x>int g) (x>int b) (x>int (* a 255)))))))

(define (mk-blend-op color-spec #!key (blend-mode 'normal) (alpha #f))
  (let-values (((rm gm bm am) (parse-color color-spec alpha)))
    (let* ((mk-rgb-op
            (alist-ref
              blend-mode
              `((normal . ,normal-op) (dissolve . ,dissolve-op)
                (multiply . ,multiply-op) (screen . ,screen-op)
                (overlay . ,overlay-op) (hard-light . ,hard-light-op)
                (soft-light . ,soft-light-op) (dodge . ,dodge-op)
                (burn . ,burn-op) (divide . ,divide-op)
                (difference . ,difference-op) (addition . ,addition-op)
                (subtract . ,subtract-op) (darken-only . ,darken-only-op)
                (lighten-only . ,lighten-only-op) (grain-extract . ,grain-extract-op)
                (grain-merge . ,grain-merge-op))))
           (mk-hsv-op
             (and (not mk-rgb-op)
                  (or (alist-ref
                        blend-mode
                        `((color . ,color-op)
                          (hue . ,hue-op)
                          (saturation . ,saturation-op)
                          (value . ,value-op)))
                      (error (sprintf "Invalid blend mode: '~A'" blend-mode)))))
           (composite
             (composite-op am))
           (blend
             (if mk-rgb-op
               (let ((rop (mk-rgb-op rm))
                     (gop (mk-rgb-op gm))
                     (bop (mk-rgb-op bm)))
                 (lambda (ri gi bi) (values (rop ri) (gop gi) (bop bi)))
               (mk-hsv-op
                 (let-values (((hm sm vm) (rgb>hsv rm gm bm)))
                   (let ((base-op (mk-hsv-op hm sm vm)))
                     (lambda (ri gi bi)
                       (let-values (((hi si vi) (rgb>hsv ri gi bi)))
                         (let-values (((h s v) (base-op hi si vi)))
                           (hsv>rgb h s v)))))))))))
      (lambda (ri gi bi ai)
        (let-values (((rb gb bb) (blend ri gi bi)))
          (composite ri gi bi ai rb gb bb am))))))
                    

(define (colorize src-img pixel-op)
  (let* ((width (image-width src-img))
         (height (image-height src-img))
         (dest (image-create width height)))
    (let hloop ((x 0))
      (when (< x width)
        (let vloop ((y 0))
          (when (< y height)
            (let-values (((ri gi bi ai) (image-pixel/rgba src-img x y)))
              (let ((final-color (pixel-op ri gi bi ai)))
                (image-draw-pixel dest final-color x y)))
          (vloop (+ y 1))))
        (hloop (+ x 1))))
    dest))

