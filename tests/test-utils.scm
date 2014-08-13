(use test)
(use imlib2)

(define (zpad x digits)
  (let* ((xstr (number->string x))
         (padlen (max (- digits (string-length xstr)) 0)))
    (string-append (make-string padlen #\0) xstr)))

(define (collect-values f . args)
  (call-with-values
    (lambda () (apply f args))
    (lambda args* args*)))

(define (collect-values* thunk)
  (call-with-values
    thunk
    (lambda args args)))

; (current-test-epsilon 0.001)
(define (list= l1 l2)
  (and (= (length l1) (length l2))
       (let loop ((l1* l1) (l2* l2))
         (cond
           ((null? l1*) #t)
           ((= (car l1*) (car l2*)) (loop (cdr l1*) (cdr l2*)))
           (else #f)))))

(define (fuzzy= x y)
  (let* ((diff (abs (- x y))))
    (<= diff (current-test-epsilon))))

(define (list-fuzzy= l1 l2)
  (and (= (length l1) (length l2))
       (let loop ((l1* l1) (l2* l2))
         (cond
           ((null? l1*) #t)
           ((fuzzy= (car l1*) (car l2*)) (loop (cdr l1*) (cdr l2*)))
           (else #f)))))

(define (list-equal? l1 l2)
  (and (= (length l1) (length l2))
       (let loop ((l1* l1) (l2* l2))
         (cond
           ((null? l1*) #t)
           ((equal? (car l1*) (car l2*)) (loop (cdr l1*) (cdr l2*)))
           (else #f)))))

(define (list-approx= l1 l2)
  (let ((approx= (lambda (x y) (< (abs (- x y)) 0.01))))
    (and (= (length l1) (length l2))
         (let loop ((l1* l1) (l2* l2))
           (cond
             ((null? l1*) #t)
             ((approx= (car l1*) (car l2*)) (loop (cdr l1*) (cdr l2*)))
             (else #f))))))

(define (legal255 _ x)
  (and (>= x 0) (<= x 255)))

(define (legal1 _ x)
  (and (>= x 0) (<= x 1)))

(define (legal-hsv _ hsv)
  (let ((h (car hsv))
        (s (cadr hsv))
        (v (caddr hsv)))
    (and (>= h 0) (<= h 360)
         (>= s 0) (<= s 1)
         (>= v 0) (<= v 1))))

(define (illegal _ _)
  #f)

(define-syntax with-comparator
  (syntax-rules ()
    ((_ comp proc ...)
     (let ((default-comparator (current-test-comparator)))
       (current-test-comparator comp)
       proc
       ...
       (current-test-comparator default-comparator)))))

(define-syntax with-epsilon
  (syntax-rules ()
    ((_ eps proc ...)
     (let ((default-epsilon (current-test-epsilon)))
       (current-test-epsilon eps)
       proc
       ...
       (current-test-epsilon default-epsilon)))))

(define (sample-pixels img x0 xstep #!key (y0 #f) (ystep #f))
  (let ((y0 (or y0 x0))
        (ystep (or ystep xstep))
        (width (image-width img))
        (height (image-height img)))
    (let loop ((x x0) (y y0) (rows '()) (row-pxx '()))
      (cond
        ((>= y height) (reverse rows)) 
        ((>= x width) (loop 0 (+ y ystep) (cons (reverse row-pxx) rows) '()))
        (else (loop (+ x xstep) y rows (cons (collect-values image-pixel/rgba img x y) row-pxx)))))))

(define default-blend-modes
  '("addition" "burn" "color" "darken_only" "difference"
    "divide" "dodge" "grain_extract" "grain_merge" "hardlight"
    "hue" "lighten_only" "multiply" "normal" "saturation"
    "screen" "subtract" "value")) 

(define default-colors
  '("000000" "0000ff" "00ff00" "27249c" "54fa0d" "5c5c5c" "775acf" "7f7d0a"
    "808080" "899675" "98d5e4" "b5b5b5" "cd1f3c" "f62db6" "ff0000" "ffffff"))

(define default-alpha-levels
  '("0.2" "0.43" "0.6" "0.78" "1.0"))

(define (mk-img-path dir blend color alpha #!optional (base "colors.png"))
  (foldl make-pathname dir `(,blend ,color ,alpha ,base)))
