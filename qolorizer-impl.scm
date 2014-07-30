(import data-structures)
(import extras)
(use imlib2)

(define (check1 x)
  (or (and (>= x 0) (<= x 1))
      (error (sprintf "Invalid input: ~A\n" x))))

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

(define (x>int255 x)
  (clamp255 (inexact->exact (round (* x 255)))))

(define (clamp1 x)
  (min (max x 0) 1))

(define (clamp255 x)
  (min (max x 0) 255))

;; Got these formulae from http://www.cs.rit.edu/~ncs/color/t_convert.html
;; I'm not positive they're correct. Alternative sources include:
;;   http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
;;   http://www.easyrgb.com/index.php?X=MATH&H=21#text21
;;   http://stackoverflow.com/questions/3018313/\
;;     algorithm-to-convert-rgb-to-hsv-and-hsv-to-rgb-in-range-0-255-for-both 

;; FIXME: pretty sloppy results here!
(define (hsv>rgb h s v)
  (when (> h 360) (error (sprintf "Hue > 360? [~A]" h)))
  (when (< h 0) (error (sprintf "Hue < 0? [~A]" h)))
  (if (= s 0)
    (values v v v)
    (let* ((h (if (= h 360) 0 (/ h 60)))
           (i (x>int (floor h)))
           (f (- h i))
           (p (* v (- 1 s)))
           (q (* v (- 1 (* s f))))
           (t (* v (- 1 (* s (- 1 f))))))
      (case i
        ((0) (values v t p))
        ((1) (values q v p))
        ((2) (values p v t))
        ((3) (values p q v))
        ((4) (values t p v))
        (else (values v p q))))))

;; ??? produces integer value for H - is that right?
(define (rgb>hsv r g b)
  (let* ((cmax (max r g b))
         (cmin (min r g b))
         (delta (- cmax cmin))
         (v cmax))
    (if (= delta 0)
      (values 0 0 v)
      (let ((h
             (x>int
               (*
                 60
                 (cond
                   ((= cmax r) (/ (- g b) delta))
                   ((= cmax g) (+ (/ (- b r) delta) 2))
                   (else (+ (/ (- r g) delta) 4))))))
            (s (/ delta cmax)))
        (if (< h 0)
          (values (+ h 360) s v)
          (values h s v))))))

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
          (values (* (/ r 255) a) (* (/ g 255) a) (* (/ b 255) a) a)))
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
            (let ((a (or (and a* (s>n a*))
                         (and alpha (verify alpha 1))
                         1.0)))
              (values (* (s>n r*) a) (* (s>n g*) a) (* (s>n b*) a) a)))))
      ((string? spec)
       (let* ((parts (map string->number (string-split spec ",")))
              (len (length parts)))
         (if (or (= len 3) (= len 4))
           (parse-color parts alpha)
           (badspec))))
      (else
        (badspec)))))

(define (normal-op m)
  (check1 m) 
  (lambda (i) (check1 i) m))

(define (dissolve-op m)
  (check1 m) 
  (lambda (i) (check1 i) (if (= (random 2) 0) i m)))

(define (multiply-op m)
  (check1 m) 
  (lambda (i) (check1 i) (* i m)))

(define (screen-op m)
  (check1 m) 
  (let ((a (- 1 m)))
    (lambda (i)
      (check1 i)
      (- 1 (* a (- 1 i))))))

(define (overlay-op m)
  (check1 m) 
  (let ((a (* 2 m)))
    (lambda (i)
      (check1 i)
      (* i (+ i (* a (- 1 i)))))))

(define (hard-light-op m)
  (check1 m) 
  (let ((a (* 2 (- m 0.5))))
    (lambda (i)
      (check1 i)
      (if (> m 0.5)
        (- 1 (* (- 1 a) (- 1 i)))
        (* 2 m i)))))

;; This formula seems completely broken
; (define (soft-light i m)
  ; (check1/2 i m)
  ; (x>int (* (/ (+ (* (- 255 i) m) (screen i m)) 255) i)))

;; Here is the W3C formula
(define (soft-light-op m)
  (check1 m) 
  (let ((a (- (* 2 m) 1))
        (b (- 1 (* 2 m))))
    (lambda (i)
      (check1 i)
      (cond
        ((and (> m 0.5) (> i 0.25))
          (+ i (* a (- (sqrt i) i))))
        ((> m 0.5)
          (+ i (* a (- (* (+ (* (- (* 16 i) 12) i) 4) i) i))))
        (else
          (- i (* b i (- 1 i))))))))

;; ???
(define (dodge-op m)
  (check1 m) 
  (let ((a (- 1 m)))
    (lambda (i) (check1 i) (if (= a 0) 1 (clamp1 (/ i a))))))

;; ???
(define (burn-op m)
  (check1 m) 
  (lambda (i)
    (check1 i)
    (if (<= m 0)
      0
      (- 1 (min 1 (/ (- 1 i) m))))))

;; 1. Based on test results, the formula in the GIMP manual matches
;;    program behavior, except values are constrained to 0 <= x <= 255.
(define (divide-op m)
  (check1 m) 
  (lambda (i)
    (check1 i)
    (if (= m 0) 1 (clamp1 (/ i m)))))

(define (difference-op m)
  (check1 m) 
  (lambda (i) (check1 i) (abs (- i m))))

(define (addition-op m)
  (check1 m) 
  (lambda (i) (check1 i) (min (+ i m) 1)))

(define (subtract-op m)
  (check1 m) 
  (lambda (i) (check1 i) (max (- i m) 0)))

; (define darken-only min)

; (define lighten-only max)

;; Temporary defs for debugging
(define (darken-only-op m)
  (check1 m) 
  (lambda (i) (check1 i) (min i m)))

(define (lighten-only-op m)
  (check1 m) 
  (lambda (i) (check1 i) (max i m)))

(define (grain-extract-op m)
  (check1 m) 
  (lambda (i)
    (check1 i)
    (max (min (+ (- i m) 0.5) 1) 0)))

(define (grain-merge-op m)
  (check1 m) 
  (lambda (i)
    (check1 i)
    (max (min (- (+ i m) 0.5) 1) 0)))

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
                        `((color . ,color-op) (hue . ,hue-op)
                          (saturation . ,saturation-op) (value . ,value-op)))
                      (error (sprintf "Invalid blend mode: '~A'" blend-mode)))))
           (blend
             (if mk-rgb-op
               (let ((rop (mk-rgb-op rm))
                     (gop (mk-rgb-op gm))
                     (bop (mk-rgb-op bm)))
                 (lambda (ri gi bi) (values (rop ri) (gop gi) (bop bi))))
               (let-values (((hm sm vm) (rgb>hsv rm gm bm)))
                 (let ((base-op (mk-hsv-op hm sm vm)))
                   (lambda (ri gi bi)
                     (let-values (((hi si vi) (rgb>hsv ri gi bi)))
                       (let-values (((h s v) (base-op hi si vi)))
                         (hsv>rgb h s v)))))))))
      (lambda (ri gi bi)
        (let-values (((rb gb bb) (blend (* ri ai) (* gi ai) (* bi ai))))
          ; Divide by ai or am??
          (let-values (((r g b a)
                        (values (x>int255 (/ rb ai)) (x>int255 (/ gb ai)) (x>int255 (/ bb ai)) (x>int255 am))))
            (color/rgba r g b a)))))))) 

(define (colorize src-img pixel-op)
  (let* ((width (image-width src-img))
         (height (image-height src-img))
         (mask (image-create width height)))
    (let hloop ((x 0))
      (when (< x width)
        (let vloop ((y 0))
          (when (< y height)
            (let-values (((ri* gi* bi* ai*) (image-pixel/rgba src-img x y)))
              (let-values (((ri gi bi) (values (/ ri* 255) (/ gi* 255) (/ bi* 255))))
                (let ((final-color (pixel-op ri gi bi)))
                  (image-draw-pixel mask final-color x y))))
          (vloop (+ y 1))))
        (hloop (+ x 1))))
    (image-blend src-img mask)))
