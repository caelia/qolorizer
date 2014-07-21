(module colorizer
        *            ; exporting all just for development
        ; (colorize)
        
        (import scheme chicken)
        (import data-structures)
        (import extras)
        (use imlib2)

(define (x>int x)
  (inexact->exact (round x)))

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
  (let ((s>n (lambda (s) (string->number (string-append "#x" s))))
        (c>s (lambda (c) (list->string (list c c))))
        (badspec (lambda () (error (sprintf "Invalid color spec: '~A'" spec)))))
    (cond
      ((list? spec)
        (let ((a (or alpha
                     (and (= (length spec) 4)
                          (cadddr spec))
                     1.0)))
          (values (car spec) (cadr spec) (caddr spec) a)))
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
            (values (s>n r*) (s>n b*) (s>n g*)
                    (or (and a* (s>n a*))
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

(define (normal i m)
  m)

(define (dissolve i m)
  (if (= (random 2) 0) i m))

(define (multiply i m)
  (x>int (/ (* i m) 255)))

(define (screen i m)
  (x>int (- 255 (/ (* (- 255 m) (- 255 i)) 255))))

(define (overlay i m)
  (x>int (* (/ i 255) (+ i (* (/ (* 2 m) 255) (- 255 i))))))

(define (hard-light i m)
  (x>int
    (if (> m 128)
      (- 255 (/ (* (- 255 (* 2 (- m 128))) (- 255 i)) 256))
      (/ (* 2 m i) 256))))

(define (soft-light i m)
  (x>int (* (/ (+ (* (- 255 i) m) (screen i m)) 255) i)))

(define (dodge i m)
  (x>int (/ (* 256 i) (+ (- 255 m) 1))))

(define (burn i m)
  (x>int (- 255 (/ (* 256 (- 255 i)) (+ m 1)))))

(define (divide i m)
  (x>int (/ (* 256 i) (+ m 1))))

(define (difference i m)
  (abs (- i m)))

(define (addition i m)
  (min (+ i m) 255))

(define (subtract i m)
  (max (- i m) 0))

(define darken-only min)

(define lighten-only max)

(define (grain-extract i m)
  (max (min (+ (- i m) 128) 255) 0))

(define (grain-merge i m)
  (max (min (- (+ i m) 128) 255) 0))

(define (color hi si vi hm sm vm)
  (values hm sm vi))

(define (hue hi si vi hm sm vm)
  (if (= sm 0)
    (values hi si vi)
    (values hm si vi)))
  
(define (saturation hi si vi hm sm vm)
  (values hi sm vi))

(define (value hi si vi hm sm vm)
  (values hi si vm))

(define (blend/rgb ri gi bi rm gm bm mode)
  (let ((op (alist-ref
              mode
              `((normal . ,normal) (dissolve . ,dissolve)
                (multiply . ,multiply) (screen . ,screen)
                (overlay . ,overlay) (hard-light . ,hard-light)
                (soft-light . ,soft-light) (dodge . ,dodge)
                (burn . ,burn) (divide . ,divide)
                (difference . ,difference) (addition . ,addition)
                (subtract . ,subtract) (darken-only . ,darken-only)
                (lighten-only . ,lighten-only) (grain-extract . ,grain-extract)
                (grain-merge . ,grain-merge)))))
    (values (x>int (op ri rm)) (x>int (op gi gm)) (x>int (op bi bm)))))

(define (blend/hsv ri gi bi rm gm bm mode)
  (let-values (((hi si vi) (rgb>hsv ri gi bi))
               ((hm sm vm) (rgb>hsv rm gm bm)))
    (let ((op (alist-ref
               mode
               `((color . ,color)
                 (hue . ,hue)
                 (saturation . ,saturation)
                 (value . ,value)))))
      (let-values (((h s v) (op hi si vi hm sm vm)))
        (hsv>rgb h s v))))) 

(define (source-over ri gi bi ai rm gm bm am)
  (let* ((a (- 1 (* (- 1 am) (- 1 ai))))
         (f
           (lambda (vi vm)
             (/ (+ (* vm am) (* vi ai (- 1 am))) a))))
    (values (f ri rm) (f gi gm) (f bi bm) a)))

(define (composite ri gi bi ai rm gm bm am #!optional (method 'source-over))
  (let-values (((r g b a)
                (case method
                  ((source-over) (source-over ri gi bi ai rm gm bm am))
                  (else (error (sprintf "Unsupported compositing method: '~A'" method))))))
    (color/rgba (x>int r) (x>int g) (x>int b) (x>int (* a 255)))))

(define (colorize src-img color-spec #!key (blend-mode 'normal) (alpha #f))
  (let* ((width (image-width src-img))
         (height (image-height src-img))
         (dest (image-create width height))
         (blend-class
           (cond
             ((memv
               blend-mode
               '(normal dissolve multiply screen overlay hard-light
                 soft-light dodge burn divide difference addition
                 subtract darken-only lighten-only grain-extract grain-merge))
               'rgb)
             ((memv
               blend-mode
               '(color hue saturation value))
               'hsv)
             (else
               (error (sprintf "Unknown blend mode: '~A'" blend-mode))))))
    (let-values (((rm gm bm am) (parse-color color-spec alpha)))
      (let hloop ((x 0))
        (when (< x width)
          (let vloop ((y 0))
            (when (< y height)
              (let-values (((ri gi bi ai) (image-pixel/rgba src-img x y)))
                (let-values (((rb gb bb)
                              (case blend-class
                                ((rgb) (blend/rgb ri gi bi rm gm bm blend-mode))
                                ((hsv) (blend/hsv ri gi bi rm gm bm blend-mode)))))
                (let ((final-color (composite ri gi bi (/ ai 255) rb rb bb am)))
                  (image-draw-pixel dest final-color x y))))
              (vloop (+ y 1))))
          (hloop (+ x 1)))))
    dest))

) ; END MODULE
