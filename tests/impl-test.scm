(use test)
(include "../colorizer-impl.scm")

(define (collect-values f . args)
  (call-with-values
    (lambda () (apply f args))
    (lambda args* args*)))

(define (list= l1 l2)
  (and (= (length l1) (length l2))
       (let loop ((l1* l1) (l2* l2))
         (cond
           ((null? l1*) #t)
           ((= (car l1*) (car l2*)) (loop (cdr l1*) (cdr l2*)))
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

(test-group "[1] parse-color: bad input causes errors"
  (test-error
    "1.01: (parse-color \"#00000000\" 1.1) [invalid alpha] => ERROR"
    (collect-values parse-color "#00000000" 1.1))
  (test-error
    "1.02: (parse-color \"#garbage\" 1.1) [nonsense color spec] => ERROR"
    (collect-values parse-color "#garbage" 1.1))
  (test-error
    "1.03: (parse-color \"garbage\" #f) [nonsense color spec] => ERROR"
    (collect-values parse-color "garbage" #f))
  (test-error
    "1.04: (parse-color '(0 0 256) #f) [value out of range] => ERROR"
    (collect-values parse-color '(0 0 256) #f))
  (test-error
    "1.05: (parse-color '(0 -19 255) #f) [value out of range] => ERROR"
    (collect-values parse-color '(0 -19 255) #f))
  (test-error
    "1.06: (parse-color \"0,0,256\" #f) [value out of range] => ERROR"
    (collect-values parse-color "0,0,256" #f))
  (test-error
    "1.07: (parse-color \"0,-19,255\" #f) [value out of range] => ERROR"
    (collect-values parse-color "0,-19,255" #f))
  (test-error
    "1.08: (parse-color \"#fa28c71\" 1.0) [wrong length] => ERROR"
    (collect-values parse-color "#fa28c71" 1.0))
  (test-error
    "1.09: (parse-color \"#39e4b\" 1.0) [wrong length] => ERROR"
    (collect-values parse-color "#39e4b" 1.0))
  (test-error
    "1.10: (parse-color \"#aa\" 1.0) [wrong length] => ERROR"
    (collect-values parse-color "#aa" 1.0))
)

(test-group "[2] parse-color: correct results"
  (with-comparator list=
    (test-group "[2.01] hex strings" 
      (test
        "2.01.01: (parse-color \"#000000\" #f) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "#000000" #f))
      (test
        "2.01.02: (parse-color \"#000000ff\" #f) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "#000000ff" #f))
      (test
        "2.01.03: (parse-color \"#000000\" 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "#000000" 1.0))
      (test
        "2.01.04: (parse-color \"#000000ff\" 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "#000000ff" 1.0))
      (test
        "2.01.05: (parse-color \"#00000000\" 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "#00000000" 1.0))
      (test
        "2.01.06: (parse-color \"#000000\" 0) => '(0 0 0 0.0)"
        '(0 0 0 0.0)
        (collect-values parse-color "#000000" 0))
      (test
        "2.01.07: (parse-color \"#000000ff\" 0) => '(0 0 0 0.0)"
        '(0 0 0 0.0)
        (collect-values parse-color "#000000ff" 0))
      (test
        "2.01.08: (parse-color \"#000\" #f) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "#000" #f))
      (test
        "2.01.09: (parse-color \"#fff\" #f) => '(255 255 255 1.0)"
        '(255 255 255 1.0)
        (collect-values parse-color "#fff" #f))
      (test
        "2.01.10: (parse-color \"#a7d733\" #f) => '(167 215 51 1.0)"
        '(167 215 51 1.0)
        (collect-values parse-color "#a7d733" #f))
      (with-comparator list-approx=
        (test
          "2.01.11: (parse-color \"#a7d73380\" #f) => '(167 215 51 0.5)"
          '(167 215 51 0.5)
          (collect-values parse-color "#a7d73380" #f))
        (test
          "2.01.12: (parse-color \"#a7d7334b\" #f) => '(167 215 51 0.294)"
          '(167 215 51 0.294)
          (collect-values parse-color "#a7d7334b" #f)))
      (test
        "2.01.13: (parse-color \"#6ac\" #f) => '(102 170 204 1.0)"
        '(102 170 204 1.0)
        (collect-values parse-color "#6ac" #f))
      (test
        "2.01.14: (parse-color \"#823f\" #f) => '(136 34 51 1.0)"
        '(136 34 51 1.0)
        (collect-values parse-color "#823f" #f))
      (test
        "2.01.15: (parse-color \"#823f\" 0.4) => '(136 34 51 0.4)"
        '(136 34 51 0.4)
        (collect-values parse-color "#823f" 0.4))
    )
    (test-group "[2.02] value lists" 
      (test
        "2.02.01: (parse-color '(0 0 0) #f) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color '(0 0 0) #f))
      (test
        "2.02.02: (parse-color '(0 0 0 1) #f) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color '(0 0 0 1) #f))
      (test
        "2.02.03: (parse-color '(0 0 0) 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color '(0 0 0) 1.0))
      (test
        "2.02.04: (parse-color '(0 0 0 1) 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color '(0 0 0 1) 1.0))
      (test
        "2.02.05: (parse-color '(0 0 0 0) 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color '(0 0 0 0) 1.0))
      (test
        "2.02.06: (parse-color '(0 0 0) 0) => '(0 0 0 0.0)"
        '(0 0 0 0.0)
        (collect-values parse-color '(0 0 0) 0))
      (test
        "2.02.07: (parse-color '(0 0 0 1) 0) => '(0 0 0 0.0)"
        '(0 0 0 0.0)
        (collect-values parse-color '(0 0 0 1) 0))
      (test
        "2.02.10: (parse-color '(167 215 51) #f) => '(167 215 51 1.0)"
        '(167 215 51 1.0)
        (collect-values parse-color '(167 215 51) #f))
      (with-comparator list-approx=
        (test
          "2.02.11: (parse-color '(167 215 51 0.5) #f) => '(167 215 51 0.5)"
          '(167 215 51 0.5)
          (collect-values parse-color '(167 215 51 0.5) #f))
        (test
          "2.02.12: (parse-color '(167 215 51 0.294) #f) => '(167 215 51 0.294)"
          '(167 215 51 0.294)
          (collect-values parse-color '(167 215 51 0.294) #f)))
    )
    (test-group "[2.03] value list strings" 
      (test
        "2.03.01: (parse-color \"0,0,0\" #f) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "0,0,0" #f))
      (test
        "2.03.02: (parse-color \"0,0,0,1\" #f) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "0,0,0,1" #f))
      (test
        "2.03.03: (parse-color \"0,0,0\" 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "0,0,0" 1.0))
      (test
        "2.03.04: (parse-color \"0,0,0,1\" 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "0,0,0,1" 1.0))
      (test
        "2.03.05: (parse-color \"0,0,0,0\" 1.0) => '(0 0 0 1.0)"
        '(0 0 0 1.0)
        (collect-values parse-color "0,0,0,0" 1.0))
      (test
        "2.03.06: (parse-color \"0,0,0\" 0) => '(0 0 0 0.0)"
        '(0 0 0 0.0)
        (collect-values parse-color "0,0,0" 0))
      (test
        "2.03.07: (parse-color \"0,0,0,1\" 0) => '(0 0 0 0.0)"
        '(0 0 0 0.0)
        (collect-values parse-color "0,0,0,1" 0))
      (test
        "2.03.10: (parse-color \"167,215,51\" #f) => '(167 215 51 1.0)"
        '(167 215 51 1.0)
        (collect-values parse-color "167,215,51" #f))
      (with-comparator list-approx=
        (test
          "2.03.11: (parse-color \"167,215,51,0.5\" #f) => '(167 215 51 0.5)"
          '(167 215 51 0.5)
          (collect-values parse-color "167,215,51,0.5" #f))
        (test
          "2.03.12: (parse-color \"167,215,51,0.294\" #f) => '(167 215 51 0.294)"
          '(167 215 51 0.294)
          (collect-values parse-color "167,215,51,0.294" #f)))
    )
))

(test-group "[3] RGB blend ops produce legal results w/ legal input"
  (with-comparator legal255
    (test-group "[3.01] normal"
      (test
        "3.01.01: (normal 0 0)"
        #:LEGAL
        (normal 0 0))
      (test
        "3.01.02: (normal 17 17)"
        #:LEGAL
        (normal 17 17))
      (test
        "3.01.03: (normal 124 124)"
        #:LEGAL
        (normal 124 124))
      (test
        "3.01.04: (normal 255 255)"
        #:LEGAL
        (normal 255 255))
      (test
        "3.01.05: (normal 0 255)"
        #:LEGAL
        (normal 0 255))
      (test
        "3.01.06: (normal 255 0)"
        #:LEGAL
        (normal 255 0))
      (test
        "3.01.07: (normal 1 0)"
        #:LEGAL
        (normal 1 0))
      (test
        "3.01.08: (normal 0 1)"
        #:LEGAL
        (normal 0 1))
      (test
        "3.01.09: (normal 254 255)"
        #:LEGAL
        (normal 254 255))
      (test
        "3.01.10: (normal 255 254)"
        #:LEGAL
        (normal 255 254))
      (test
        "3.01.11: (normal 29 28)"
        #:LEGAL
        (normal 29 28))
      (test
        "3.01.12: (normal 28 29)"
        #:LEGAL
        (normal 28 29))
      (test
        "3.01.13: (normal 17 204)"
        #:LEGAL
        (normal 17 204))
      (test
        "3.01.14: (normal 204 17)"
        #:LEGAL
        (normal 204 17))
      (test
        "3.01.15: (normal 59 80)"
        #:LEGAL
        (normal 59 80))
      (test
        "3.01.16: (normal 80 59)"
        #:LEGAL
        (normal 80 59)))
    (test-group "[3.02] dissolve"
      (test
        "3.02.01: (dissolve 0 0)"
        #:LEGAL
        (dissolve 0 0))
      (test
        "3.02.02: (dissolve 17 17)"
        #:LEGAL
        (dissolve 17 17))
      (test
        "3.02.03: (dissolve 124 124)"
        #:LEGAL
        (dissolve 124 124))
      (test
        "3.02.04: (dissolve 255 255)"
        #:LEGAL
        (dissolve 255 255))
      (test
        "3.02.05: (dissolve 0 255)"
        #:LEGAL
        (dissolve 0 255))
      (test
        "3.02.06: (dissolve 255 0)"
        #:LEGAL
        (dissolve 255 0))
      (test
        "3.02.07: (dissolve 1 0)"
        #:LEGAL
        (dissolve 1 0))
      (test
        "3.02.08: (dissolve 0 1)"
        #:LEGAL
        (dissolve 0 1))
      (test
        "3.02.09: (dissolve 254 255)"
        #:LEGAL
        (dissolve 254 255))
      (test
        "3.02.10: (dissolve 255 254)"
        #:LEGAL
        (dissolve 255 254))
      (test
        "3.02.11: (dissolve 29 28)"
        #:LEGAL
        (dissolve 29 28))
      (test
        "3.02.12: (dissolve 28 29)"
        #:LEGAL
        (dissolve 28 29))
      (test
        "3.02.13: (dissolve 17 204)"
        #:LEGAL
        (dissolve 17 204))
      (test
        "3.02.14: (dissolve 204 17)"
        #:LEGAL
        (dissolve 204 17))
      (test
        "3.02.15: (dissolve 59 80)"
        #:LEGAL
        (dissolve 59 80))
      (test
        "3.02.16: (dissolve 80 59)"
        #:LEGAL
        (dissolve 80 59)))
    (test-group "[3.03] multiply"
      (test
        "3.03.01: (multiply 0 0)"
        #:LEGAL
        (multiply 0 0))
      (test
        "3.03.02: (multiply 17 17)"
        #:LEGAL
        (multiply 17 17))
      (test
        "3.03.03: (multiply 124 124)"
        #:LEGAL
        (multiply 124 124))
      (test
        "3.03.04: (multiply 255 255)"
        #:LEGAL
        (multiply 255 255))
      (test
        "3.03.05: (multiply 0 255)"
        #:LEGAL
        (multiply 0 255))
      (test
        "3.03.06: (multiply 255 0)"
        #:LEGAL
        (multiply 255 0))
      (test
        "3.03.07: (multiply 1 0)"
        #:LEGAL
        (multiply 1 0))
      (test
        "3.03.08: (multiply 0 1)"
        #:LEGAL
        (multiply 0 1))
      (test
        "3.03.09: (multiply 254 255)"
        #:LEGAL
        (multiply 254 255))
      (test
        "3.03.10: (multiply 255 254)"
        #:LEGAL
        (multiply 255 254))
      (test
        "3.03.11: (multiply 29 28)"
        #:LEGAL
        (multiply 29 28))
      (test
        "3.03.12: (multiply 28 29)"
        #:LEGAL
        (multiply 28 29))
      (test
        "3.03.13: (multiply 17 204)"
        #:LEGAL
        (multiply 17 204))
      (test
        "3.03.14: (multiply 204 17)"
        #:LEGAL
        (multiply 204 17))
      (test
        "3.03.15: (multiply 59 80)"
        #:LEGAL
        (multiply 59 80))
      (test
        "3.03.16: (multiply 80 59)"
        #:LEGAL
        (multiply 80 59)))
    (test-group "[3.04] screen"
      (test
        "3.04.01: (screen 0 0)"
        #:LEGAL
        (screen 0 0))
      (test
        "3.04.02: (screen 17 17)"
        #:LEGAL
        (screen 17 17))
      (test
        "3.04.03: (screen 124 124)"
        #:LEGAL
        (screen 124 124))
      (test
        "3.04.04: (screen 255 255)"
        #:LEGAL
        (screen 255 255))
      (test
        "3.04.05: (screen 0 255)"
        #:LEGAL
        (screen 0 255))
      (test
        "3.04.06: (screen 255 0)"
        #:LEGAL
        (screen 255 0))
      (test
        "3.04.07: (screen 1 0)"
        #:LEGAL
        (screen 1 0))
      (test
        "3.04.08: (screen 0 1)"
        #:LEGAL
        (screen 0 1))
      (test
        "3.04.09: (screen 254 255)"
        #:LEGAL
        (screen 254 255))
      (test
        "3.04.10: (screen 255 254)"
        #:LEGAL
        (screen 255 254))
      (test
        "3.04.11: (screen 29 28)"
        #:LEGAL
        (screen 29 28))
      (test
        "3.04.12: (screen 28 29)"
        #:LEGAL
        (screen 28 29))
      (test
        "3.04.13: (screen 17 204)"
        #:LEGAL
        (screen 17 204))
      (test
        "3.04.14: (screen 204 17)"
        #:LEGAL
        (screen 204 17))
      (test
        "3.04.15: (screen 59 80)"
        #:LEGAL
        (screen 59 80))
      (test
        "3.04.16: (screen 80 59)"
        #:LEGAL
        (screen 80 59)))
    (test-group "[3.05] overlay"
      (test
        "3.05.01: (overlay 0 0)"
        #:LEGAL
        (overlay 0 0))
      (test
        "3.05.02: (overlay 17 17)"
        #:LEGAL
        (overlay 17 17))
      (test
        "3.05.03: (overlay 124 124)"
        #:LEGAL
        (overlay 124 124))
      (test
        "3.05.04: (overlay 255 255)"
        #:LEGAL
        (overlay 255 255))
      (test
        "3.05.05: (overlay 0 255)"
        #:LEGAL
        (overlay 0 255))
      (test
        "3.05.06: (overlay 255 0)"
        #:LEGAL
        (overlay 255 0))
      (test
        "3.05.07: (overlay 1 0)"
        #:LEGAL
        (overlay 1 0))
      (test
        "3.05.08: (overlay 0 1)"
        #:LEGAL
        (overlay 0 1))
      (test
        "3.05.09: (overlay 254 255)"
        #:LEGAL
        (overlay 254 255))
      (test
        "3.05.10: (overlay 255 254)"
        #:LEGAL
        (overlay 255 254))
      (test
        "3.05.11: (overlay 29 28)"
        #:LEGAL
        (overlay 29 28))
      (test
        "3.05.12: (overlay 28 29)"
        #:LEGAL
        (overlay 28 29))
      (test
        "3.05.13: (overlay 17 204)"
        #:LEGAL
        (overlay 17 204))
      (test
        "3.05.14: (overlay 204 17)"
        #:LEGAL
        (overlay 204 17))
      (test
        "3.05.15: (overlay 59 80)"
        #:LEGAL
        (overlay 59 80))
      (test
        "3.05.16: (overlay 80 59)"
        #:LEGAL
        (overlay 80 59)))
    (test-group "[3.06] difference"
      (test
        "3.06.01: (difference 0 0)"
        #:LEGAL
        (difference 0 0))
      (test
        "3.06.02: (difference 17 17)"
        #:LEGAL
        (difference 17 17))
      (test
        "3.06.03: (difference 124 124)"
        #:LEGAL
        (difference 124 124))
      (test
        "3.06.04: (difference 255 255)"
        #:LEGAL
        (difference 255 255))
      (test
        "3.06.05: (difference 0 255)"
        #:LEGAL
        (difference 0 255))
      (test
        "3.06.06: (difference 255 0)"
        #:LEGAL
        (difference 255 0))
      (test
        "3.06.07: (difference 1 0)"
        #:LEGAL
        (difference 1 0))
      (test
        "3.06.08: (difference 0 1)"
        #:LEGAL
        (difference 0 1))
      (test
        "3.06.09: (difference 254 255)"
        #:LEGAL
        (difference 254 255))
      (test
        "3.06.10: (difference 255 254)"
        #:LEGAL
        (difference 255 254))
      (test
        "3.06.11: (difference 29 28)"
        #:LEGAL
        (difference 29 28))
      (test
        "3.06.12: (difference 28 29)"
        #:LEGAL
        (difference 28 29))
      (test
        "3.06.13: (difference 17 204)"
        #:LEGAL
        (difference 17 204))
      (test
        "3.06.14: (difference 204 17)"
        #:LEGAL
        (difference 204 17))
      (test
        "3.06.15: (difference 59 80)"
        #:LEGAL
        (difference 59 80))
      (test
        "3.06.16: (difference 80 59)"
        #:LEGAL
        (difference 80 59)))
    (test-group "[3.07] addition"
      (test
        "3.07.01: (addition 0 0)"
        #:LEGAL
        (addition 0 0))
      (test
        "3.07.02: (addition 17 17)"
        #:LEGAL
        (addition 17 17))
      (test
        "3.07.03: (addition 124 124)"
        #:LEGAL
        (addition 124 124))
      (test
        "3.07.04: (addition 255 255)"
        #:LEGAL
        (addition 255 255))
      (test
        "3.07.05: (addition 0 255)"
        #:LEGAL
        (addition 0 255))
      (test
        "3.07.06: (addition 255 0)"
        #:LEGAL
        (addition 255 0))
      (test
        "3.07.07: (addition 1 0)"
        #:LEGAL
        (addition 1 0))
      (test
        "3.07.08: (addition 0 1)"
        #:LEGAL
        (addition 0 1))
      (test
        "3.07.09: (addition 254 255)"
        #:LEGAL
        (addition 254 255))
      (test
        "3.07.10: (addition 255 254)"
        #:LEGAL
        (addition 255 254))
      (test
        "3.07.11: (addition 29 28)"
        #:LEGAL
        (addition 29 28))
      (test
        "3.07.12: (addition 28 29)"
        #:LEGAL
        (addition 28 29))
      (test
        "3.07.13: (addition 17 204)"
        #:LEGAL
        (addition 17 204))
      (test
        "3.07.14: (addition 204 17)"
        #:LEGAL
        (addition 204 17))
      (test
        "3.07.15: (addition 59 80)"
        #:LEGAL
        (addition 59 80))
      (test
        "3.07.16: (addition 80 59)"
        #:LEGAL
        (addition 80 59)))
    (test-group "[3.08] subtract"
      (test
        "3.08.01: (subtract 0 0)"
        #:LEGAL
        (subtract 0 0))
      (test
        "3.08.02: (subtract 17 17)"
        #:LEGAL
        (subtract 17 17))
      (test
        "3.08.03: (subtract 124 124)"
        #:LEGAL
        (subtract 124 124))
      (test
        "3.08.04: (subtract 255 255)"
        #:LEGAL
        (subtract 255 255))
      (test
        "3.08.05: (subtract 0 255)"
        #:LEGAL
        (subtract 0 255))
      (test
        "3.08.06: (subtract 255 0)"
        #:LEGAL
        (subtract 255 0))
      (test
        "3.08.07: (subtract 1 0)"
        #:LEGAL
        (subtract 1 0))
      (test
        "3.08.08: (subtract 0 1)"
        #:LEGAL
        (subtract 0 1))
      (test
        "3.08.09: (subtract 254 255)"
        #:LEGAL
        (subtract 254 255))
      (test
        "3.08.10: (subtract 255 254)"
        #:LEGAL
        (subtract 255 254))
      (test
        "3.08.11: (subtract 29 28)"
        #:LEGAL
        (subtract 29 28))
      (test
        "3.08.12: (subtract 28 29)"
        #:LEGAL
        (subtract 28 29))
      (test
        "3.08.13: (subtract 17 204)"
        #:LEGAL
        (subtract 17 204))
      (test
        "3.08.14: (subtract 204 17)"
        #:LEGAL
        (subtract 204 17))
      (test
        "3.08.15: (subtract 59 80)"
        #:LEGAL
        (subtract 59 80))
      (test
        "3.08.16: (subtract 80 59)"
        #:LEGAL
        (subtract 80 59)))
    (test-group "[3.09] darken-only"
      (test
        "3.09.01: (darken-only 0 0)"
        #:LEGAL
        (darken-only 0 0))
      (test
        "3.09.02: (darken-only 17 17)"
        #:LEGAL
        (darken-only 17 17))
      (test
        "3.09.03: (darken-only 124 124)"
        #:LEGAL
        (darken-only 124 124))
      (test
        "3.09.04: (darken-only 255 255)"
        #:LEGAL
        (darken-only 255 255))
      (test
        "3.09.05: (darken-only 0 255)"
        #:LEGAL
        (darken-only 0 255))
      (test
        "3.09.06: (darken-only 255 0)"
        #:LEGAL
        (darken-only 255 0))
      (test
        "3.09.07: (darken-only 1 0)"
        #:LEGAL
        (darken-only 1 0))
      (test
        "3.09.08: (darken-only 0 1)"
        #:LEGAL
        (darken-only 0 1))
      (test
        "3.09.09: (darken-only 254 255)"
        #:LEGAL
        (darken-only 254 255))
      (test
        "3.09.10: (darken-only 255 254)"
        #:LEGAL
        (darken-only 255 254))
      (test
        "3.09.11: (darken-only 29 28)"
        #:LEGAL
        (darken-only 29 28))
      (test
        "3.09.12: (darken-only 28 29)"
        #:LEGAL
        (darken-only 28 29))
      (test
        "3.09.13: (darken-only 17 204)"
        #:LEGAL
        (darken-only 17 204))
      (test
        "3.09.14: (darken-only 204 17)"
        #:LEGAL
        (darken-only 204 17))
      (test
        "3.09.15: (darken-only 59 80)"
        #:LEGAL
        (darken-only 59 80))
      (test
        "3.09.16: (darken-only 80 59)"
        #:LEGAL
        (darken-only 80 59)))
    (test-group "[3.10] lighten-only"
      (test
        "3.10.01: (lighten-only 0 0)"
        #:LEGAL
        (lighten-only 0 0))
      (test
        "3.10.02: (lighten-only 17 17)"
        #:LEGAL
        (lighten-only 17 17))
      (test
        "3.10.03: (lighten-only 124 124)"
        #:LEGAL
        (lighten-only 124 124))
      (test
        "3.10.04: (lighten-only 255 255)"
        #:LEGAL
        (lighten-only 255 255))
      (test
        "3.10.05: (lighten-only 0 255)"
        #:LEGAL
        (lighten-only 0 255))
      (test
        "3.10.06: (lighten-only 255 0)"
        #:LEGAL
        (lighten-only 255 0))
      (test
        "3.10.07: (lighten-only 1 0)"
        #:LEGAL
        (lighten-only 1 0))
      (test
        "3.10.08: (lighten-only 0 1)"
        #:LEGAL
        (lighten-only 0 1))
      (test
        "3.10.09: (lighten-only 254 255)"
        #:LEGAL
        (lighten-only 254 255))
      (test
        "3.10.10: (lighten-only 255 254)"
        #:LEGAL
        (lighten-only 255 254))
      (test
        "3.10.11: (lighten-only 29 28)"
        #:LEGAL
        (lighten-only 29 28))
      (test
        "3.10.12: (lighten-only 28 29)"
        #:LEGAL
        (lighten-only 28 29))
      (test
        "3.10.13: (lighten-only 17 204)"
        #:LEGAL
        (lighten-only 17 204))
      (test
        "3.10.14: (lighten-only 204 17)"
        #:LEGAL
        (lighten-only 204 17))
      (test
        "3.10.15: (lighten-only 59 80)"
        #:LEGAL
        (lighten-only 59 80))
      (test
        "3.10.16: (lighten-only 80 59)"
        #:LEGAL
        (lighten-only 80 59)))
    (test-group "[3.11] divide"
      (test
        "3.11.01: (divide 0 0)"
        #:LEGAL
        (divide 0 0))
      (test
        "3.11.02: (divide 17 17)"
        #:LEGAL
        (divide 17 17))
      (test
        "3.11.03: (divide 124 124)"
        #:LEGAL
        (divide 124 124))
      (test
        "3.11.04: (divide 255 255)"
        #:LEGAL
        (divide 255 255))
      (test
        "3.11.05: (divide 0 255)"
        #:LEGAL
        (divide 0 255))
      (test
        "3.11.06: (divide 255 0)"
        #:LEGAL
        (divide 255 0))
      (test
        "3.11.07: (divide 1 0)"
        #:LEGAL
        (divide 1 0))
      (test
        "3.11.08: (divide 0 1)"
        #:LEGAL
        (divide 0 1))
      (test
        "3.11.09: (divide 254 255)"
        #:LEGAL
        (divide 254 255))
      (test
        "3.11.10: (divide 255 254)"
        #:LEGAL
        (divide 255 254))
      (test
        "3.11.11: (divide 29 28)"
        #:LEGAL
        (divide 29 28))
      (test
        "3.11.12: (divide 28 29)"
        #:LEGAL
        (divide 28 29))
      (test
        "3.11.13: (divide 17 204)"
        #:LEGAL
        (divide 17 204))
      (test
        "3.11.14: (divide 204 17)"
        #:LEGAL
        (divide 204 17))
      (test
        "3.11.15: (divide 59 80)"
        #:LEGAL
        (divide 59 80))
      (test
        "3.11.16: (divide 80 59)"
        #:LEGAL
        (divide 80 59)))
    (test-group "[3.12] dodge"
      (test
        "3.12.01: (dodge 0 0)"
        #:LEGAL
        (dodge 0 0))
      (test
        "3.12.02: (dodge 17 17)"
        #:LEGAL
        (dodge 17 17))
      (test
        "3.12.03: (dodge 124 124)"
        #:LEGAL
        (dodge 124 124))
      (test
        "3.12.04: (dodge 255 255)"
        #:LEGAL
        (dodge 255 255))
      (test
        "3.12.05: (dodge 0 255)"
        #:LEGAL
        (dodge 0 255))
      (test
        "3.12.06: (dodge 255 0)"
        #:LEGAL
        (dodge 255 0))
      (test
        "3.12.07: (dodge 1 0)"
        #:LEGAL
        (dodge 1 0))
      (test
        "3.12.08: (dodge 0 1)"
        #:LEGAL
        (dodge 0 1))
      (test
        "3.12.09: (dodge 254 255)"
        #:LEGAL
        (dodge 254 255))
      (test
        "3.12.10: (dodge 255 254)"
        #:LEGAL
        (dodge 255 254))
      (test
        "3.12.11: (dodge 29 28)"
        #:LEGAL
        (dodge 29 28))
      (test
        "3.12.12: (dodge 28 29)"
        #:LEGAL
        (dodge 28 29))
      (test
        "3.12.13: (dodge 17 204)"
        #:LEGAL
        (dodge 17 204))
      (test
        "3.12.14: (dodge 204 17)"
        #:LEGAL
        (dodge 204 17))
      (test
        "3.12.15: (dodge 59 80)"
        #:LEGAL
        (dodge 59 80))
      (test
        "3.12.16: (dodge 80 59)"
        #:LEGAL
        (dodge 80 59)))
    (test-group "[3.13] burn"
      (test
        "3.13.01: (burn 0 0)"
        #:LEGAL
        (burn 0 0))
      (test
        "3.13.02: (burn 17 17)"
        #:LEGAL
        (burn 17 17))
      (test
        "3.13.03: (burn 124 124)"
        #:LEGAL
        (burn 124 124))
      (test
        "3.13.04: (burn 255 255)"
        #:LEGAL
        (burn 255 255))
      (test
        "3.13.05: (burn 0 255)"
        #:LEGAL
        (burn 0 255))
      (test
        "3.13.06: (burn 255 0)"
        #:LEGAL
        (burn 255 0))
      (test
        "3.13.07: (burn 1 0)"
        #:LEGAL
        (burn 1 0))
      (test
        "3.13.08: (burn 0 1)"
        #:LEGAL
        (burn 0 1))
      (test
        "3.13.09: (burn 254 255)"
        #:LEGAL
        (burn 254 255))
      (test
        "3.13.10: (burn 255 254)"
        #:LEGAL
        (burn 255 254))
      (test
        "3.13.11: (burn 29 28)"
        #:LEGAL
        (burn 29 28))
      (test
        "3.13.12: (burn 28 29)"
        #:LEGAL
        (burn 28 29))
      (test
        "3.13.13: (burn 17 204)"
        #:LEGAL
        (burn 17 204))
      (test
        "3.13.14: (burn 204 17)"
        #:LEGAL
        (burn 204 17))
      (test
        "3.13.15: (burn 59 80)"
        #:LEGAL
        (burn 59 80))
      (test
        "3.13.16: (burn 80 59)"
        #:LEGAL
        (burn 80 59)))
    (test-group "[3.14] hard-light"
      (test
        "3.14.01: (hard-light 0 0)"
        #:LEGAL
        (hard-light 0 0))
      (test
        "3.14.02: (hard-light 17 17)"
        #:LEGAL
        (hard-light 17 17))
      (test
        "3.14.03: (hard-light 124 124)"
        #:LEGAL
        (hard-light 124 124))
      (test
        "3.14.04: (hard-light 255 255)"
        #:LEGAL
        (hard-light 255 255))
      (test
        "3.14.05: (hard-light 0 255)"
        #:LEGAL
        (hard-light 0 255))
      (test
        "3.14.06: (hard-light 255 0)"
        #:LEGAL
        (hard-light 255 0))
      (test
        "3.14.07: (hard-light 1 0)"
        #:LEGAL
        (hard-light 1 0))
      (test
        "3.14.08: (hard-light 0 1)"
        #:LEGAL
        (hard-light 0 1))
      (test
        "3.14.09: (hard-light 254 255)"
        #:LEGAL
        (hard-light 254 255))
      (test
        "3.14.10: (hard-light 255 254)"
        #:LEGAL
        (hard-light 255 254))
      (test
        "3.14.11: (hard-light 29 28)"
        #:LEGAL
        (hard-light 29 28))
      (test
        "3.14.12: (hard-light 28 29)"
        #:LEGAL
        (hard-light 28 29))
      (test
        "3.14.13: (hard-light 17 204)"
        #:LEGAL
        (hard-light 17 204))
      (test
        "3.14.14: (hard-light 204 17)"
        #:LEGAL
        (hard-light 204 17))
      (test
        "3.14.15: (hard-light 59 80)"
        #:LEGAL
        (hard-light 59 80))
      (test
        "3.14.16: (hard-light 80 59)"
        #:LEGAL
        (hard-light 80 59)))
    (test-group "[3.15] soft-light"
      (test
        "3.15.01: (soft-light 0 0)"
        #:LEGAL
        (soft-light 0 0))
      (test
        "3.15.02: (soft-light 17 17)"
        #:LEGAL
        (soft-light 17 17))
      (test
        "3.15.03: (soft-light 124 124)"
        #:LEGAL
        (soft-light 124 124))
      (test
        "3.15.04: (soft-light 255 255)"
        #:LEGAL
        (soft-light 255 255))
      (test
        "3.15.05: (soft-light 0 255)"
        #:LEGAL
        (soft-light 0 255))
      (test
        "3.15.06: (soft-light 255 0)"
        #:LEGAL
        (soft-light 255 0))
      (test
        "3.15.07: (soft-light 1 0)"
        #:LEGAL
        (soft-light 1 0))
      (test
        "3.15.08: (soft-light 0 1)"
        #:LEGAL
        (soft-light 0 1))
      (test
        "3.15.09: (soft-light 254 255)"
        #:LEGAL
        (soft-light 254 255))
      (test
        "3.15.10: (soft-light 255 254)"
        #:LEGAL
        (soft-light 255 254))
      (test
        "3.15.11: (soft-light 29 28)"
        #:LEGAL
        (soft-light 29 28))
      (test
        "3.15.12: (soft-light 28 29)"
        #:LEGAL
        (soft-light 28 29))
      (test
        "3.15.13: (soft-light 17 204)"
        #:LEGAL
        (soft-light 17 204))
      (test
        "3.15.14: (soft-light 204 17)"
        #:LEGAL
        (soft-light 204 17))
      (test
        "3.15.15: (soft-light 59 80)"
        #:LEGAL
        (soft-light 59 80))
      (test
        "3.15.16: (soft-light 80 59)"
        #:LEGAL
        (soft-light 80 59)))
    (test-group "[3.16] grain-extract"
      (test
        "3.16.01: (grain-extract 0 0)"
        #:LEGAL
        (grain-extract 0 0))
      (test
        "3.16.02: (grain-extract 17 17)"
        #:LEGAL
        (grain-extract 17 17))
      (test
        "3.16.03: (grain-extract 124 124)"
        #:LEGAL
        (grain-extract 124 124))
      (test
        "3.16.04: (grain-extract 255 255)"
        #:LEGAL
        (grain-extract 255 255))
      (test
        "3.16.05: (grain-extract 0 255)"
        #:LEGAL
        (grain-extract 0 255))
      (test
        "3.16.06: (grain-extract 255 0)"
        #:LEGAL
        (grain-extract 255 0))
      (test
        "3.16.07: (grain-extract 1 0)"
        #:LEGAL
        (grain-extract 1 0))
      (test
        "3.16.08: (grain-extract 0 1)"
        #:LEGAL
        (grain-extract 0 1))
      (test
        "3.16.09: (grain-extract 254 255)"
        #:LEGAL
        (grain-extract 254 255))
      (test
        "3.16.10: (grain-extract 255 254)"
        #:LEGAL
        (grain-extract 255 254))
      (test
        "3.16.11: (grain-extract 29 28)"
        #:LEGAL
        (grain-extract 29 28))
      (test
        "3.16.12: (grain-extract 28 29)"
        #:LEGAL
        (grain-extract 28 29))
      (test
        "3.16.13: (grain-extract 17 204)"
        #:LEGAL
        (grain-extract 17 204))
      (test
        "3.16.14: (grain-extract 204 17)"
        #:LEGAL
        (grain-extract 204 17))
      (test
        "3.16.15: (grain-extract 59 80)"
        #:LEGAL
        (grain-extract 59 80))
      (test
        "3.16.16: (grain-extract 80 59)"
        #:LEGAL
        (grain-extract 80 59)))
    (test-group "[3.17] grain-merge"
      (test
        "3.17.01: (grain-merge 0 0)"
        #:LEGAL
        (grain-merge 0 0))
      (test
        "3.17.02: (grain-merge 17 17)"
        #:LEGAL
        (grain-merge 17 17))
      (test
        "3.17.03: (grain-merge 124 124)"
        #:LEGAL
        (grain-merge 124 124))
      (test
        "3.17.04: (grain-merge 255 255)"
        #:LEGAL
        (grain-merge 255 255))
      (test
        "3.17.05: (grain-merge 0 255)"
        #:LEGAL
        (grain-merge 0 255))
      (test
        "3.17.06: (grain-merge 255 0)"
        #:LEGAL
        (grain-merge 255 0))
      (test
        "3.17.07: (grain-merge 1 0)"
        #:LEGAL
        (grain-merge 1 0))
      (test
        "3.17.08: (grain-merge 0 1)"
        #:LEGAL
        (grain-merge 0 1))
      (test
        "3.17.09: (grain-merge 254 255)"
        #:LEGAL
        (grain-merge 254 255))
      (test
        "3.17.10: (grain-merge 255 254)"
        #:LEGAL
        (grain-merge 255 254))
      (test
        "3.17.11: (grain-merge 29 28)"
        #:LEGAL
        (grain-merge 29 28))
      (test
        "3.17.12: (grain-merge 28 29)"
        #:LEGAL
        (grain-merge 28 29))
      (test
        "3.17.13: (grain-merge 17 204)"
        #:LEGAL
        (grain-merge 17 204))
      (test
        "3.17.14: (grain-merge 204 17)"
        #:LEGAL
        (grain-merge 204 17))
      (test
        "3.17.15: (grain-merge 59 80)"
        #:LEGAL
        (grain-merge 59 80))
      (test
        "3.17.16: (grain-merge 80 59)"
        #:LEGAL
        (grain-merge 80 59)))
))

(test-group "[4] HSV blend ops produce legal results w/ legal input"
  (with-comparator legal-hsv
    (test-group "[4.01] color"
      (test
        "4.01.01: (color 0 0 0 0 0 0)"
        #:LEGAL
        (collect-values color 0 0 0 0 0 0))
      (test
        "4.01.02: (color 0 1 1 0 0 0)"
        #:LEGAL
        (collect-values color 0 1 1 0 0 0 ))
      (test
        "4.01.03: (color 0 1 1 0 1 1)"
        #:LEGAL
        (collect-values color 0 1 1 0 1 1))
      (test
        "4.01.04: (color 360 0 0 0 0 0)"
        #:LEGAL
        (collect-values color 360 0 0 0 0 0))
      (test
        "4.01.05: (color 360 1 1 360 1 1)"
        #:LEGAL
        (collect-values color 360 1 1 360 1 1))
      (test
        "4.01.06: (color 360 0 0 0 1 1)"
        #:LEGAL
        (collect-values color 360 0 0 0 1 1))
      (test
        "4.01.07: (color 360 1 1 360 0 0)"
        #:LEGAL
        (collect-values color 360 1 1 360 0 0))
      (test
        "4.01.08: (color 118 0.5 0.25 118 0.5 0.25)"
        #:LEGAL
        (collect-values color 118 0.5 0.25 118 0.5 0.25))
      (test
        "4.01.09: (color 118 0.5 0.25 99 1 1)"
        #:LEGAL
        (collect-values color 118 0.5 0.25 99 1 1))
      (test
        "4.01.10: (color 118 0.5 0.25 99 0 0)"
        #:LEGAL
        (collect-values color 118 0.5 0.25 99 0 0))
      (test
        "4.01.11: (color 271 0.01 0.002 44 1 0.007)"
        #:LEGAL
        (collect-values color 271 0.01 0.002 44 1 0.007))
      (test
        "4.01.12: (color 224 0.999 0.9 31 1 1)"
        #:LEGAL
        (collect-values color 224 0.999 0.9 31 1 1))
      (test
        "4.01.13: (color 88 0 1 88 0.02 0.93)"
        #:LEGAL
        (collect-values color 88 0 1 88 0.02 0.93))
      (test
        "4.01.14: (color 88 0 1 88 1 0)"
        #:LEGAL
        (collect-values color 88 0 1 88 1 0))
      (test
        "4.01.15: (color 74 1 0 75 0.95 0.0001)"
        #:LEGAL
        (collect-values color 74 1 0 75 0.95 0.0001))
      (test
        "4.01.16: (color 144 1.0 0.999 299 0.65 0.07)"
        #:LEGAL
        (collect-values color 144 1.0 0.999 299 0.65 0.07))
      (test
        "4.01.17: (color 243 0.0007 0.4 248 0.002 1.0)"
        #:LEGAL
        (collect-values color 243 0.0007 0.4 248 0.002 1.0)))
    (test-group "[4.02] hue"
      (test
        "4.02.01: (hue 0 0 0 0 0 0)"
        #:LEGAL
        (collect-values hue 0 0 0 0 0 0))
      (test
        "4.02.02: (hue 0 1 1 0 0 0)"
        #:LEGAL
        (collect-values hue 0 1 1 0 0 0 ))
      (test
        "4.02.03: (hue 0 1 1 0 1 1)"
        #:LEGAL
        (collect-values hue 0 1 1 0 1 1))
      (test
        "4.02.04: (hue 360 0 0 0 0 0)"
        #:LEGAL
        (collect-values hue 360 0 0 0 0 0))
      (test
        "4.02.05: (hue 360 1 1 360 1 1)"
        #:LEGAL
        (collect-values hue 360 1 1 360 1 1))
      (test
        "4.02.06: (hue 360 0 0 0 1 1)"
        #:LEGAL
        (collect-values hue 360 0 0 0 1 1))
      (test
        "4.02.07: (hue 360 1 1 360 0 0)"
        #:LEGAL
        (collect-values hue 360 1 1 360 0 0))
      (test
        "4.02.08: (hue 118 0.5 0.25 118 0.5 0.25)"
        #:LEGAL
        (collect-values hue 118 0.5 0.25 118 0.5 0.25))
      (test
        "4.02.09: (hue 118 0.5 0.25 99 1 1)"
        #:LEGAL
        (collect-values hue 118 0.5 0.25 99 1 1))
      (test
        "4.02.10: (hue 118 0.5 0.25 99 0 0)"
        #:LEGAL
        (collect-values hue 118 0.5 0.25 99 0 0))
      (test
        "4.02.11: (hue 271 0.01 0.002 44 1 0.007)"
        #:LEGAL
        (collect-values hue 271 0.01 0.002 44 1 0.007))
      (test
        "4.02.12: (hue 224 0.999 0.9 31 1 1)"
        #:LEGAL
        (collect-values hue 224 0.999 0.9 31 1 1))
      (test
        "4.02.13: (hue 88 0 1 88 0.02 0.93)"
        #:LEGAL
        (collect-values hue 88 0 1 88 0.02 0.93))
      (test
        "4.02.14: (hue 88 0 1 88 1 0)"
        #:LEGAL
        (collect-values hue 88 0 1 88 1 0))
      (test
        "4.02.15: (hue 74 1 0 75 0.95 0.0001)"
        #:LEGAL
        (collect-values hue 74 1 0 75 0.95 0.0001))
      (test
        "4.02.16: (hue 144 1.0 0.999 299 0.65 0.07)"
        #:LEGAL
        (collect-values hue 144 1.0 0.999 299 0.65 0.07))
      (test
        "4.02.17: (hue 243 0.0007 0.4 248 0.002 1.0)"
        #:LEGAL
        (collect-values hue 243 0.0007 0.4 248 0.002 1.0)))
    (test-group "[4.03] saturation"
      (test
        "4.03.01: (saturation 0 0 0 0 0 0)"
        #:LEGAL
        (collect-values saturation 0 0 0 0 0 0))
      (test
        "4.03.02: (saturation 0 1 1 0 0 0)"
        #:LEGAL
        (collect-values saturation 0 1 1 0 0 0 ))
      (test
        "4.03.03: (saturation 0 1 1 0 1 1)"
        #:LEGAL
        (collect-values saturation 0 1 1 0 1 1))
      (test
        "4.03.04: (saturation 360 0 0 0 0 0)"
        #:LEGAL
        (collect-values saturation 360 0 0 0 0 0))
      (test
        "4.03.05: (saturation 360 1 1 360 1 1)"
        #:LEGAL
        (collect-values saturation 360 1 1 360 1 1))
      (test
        "4.03.06: (saturation 360 0 0 0 1 1)"
        #:LEGAL
        (collect-values saturation 360 0 0 0 1 1))
      (test
        "4.03.07: (saturation 360 1 1 360 0 0)"
        #:LEGAL
        (collect-values saturation 360 1 1 360 0 0))
      (test
        "4.03.08: (saturation 118 0.5 0.25 118 0.5 0.25)"
        #:LEGAL
        (collect-values saturation 118 0.5 0.25 118 0.5 0.25))
      (test
        "4.03.09: (saturation 118 0.5 0.25 99 1 1)"
        #:LEGAL
        (collect-values saturation 118 0.5 0.25 99 1 1))
      (test
        "4.03.10: (saturation 118 0.5 0.25 99 0 0)"
        #:LEGAL
        (collect-values saturation 118 0.5 0.25 99 0 0))
      (test
        "4.03.11: (saturation 271 0.01 0.002 44 1 0.007)"
        #:LEGAL
        (collect-values saturation 271 0.01 0.002 44 1 0.007))
      (test
        "4.03.12: (saturation 224 0.999 0.9 31 1 1)"
        #:LEGAL
        (collect-values saturation 224 0.999 0.9 31 1 1))
      (test
        "4.03.13: (saturation 88 0 1 88 0.02 0.93)"
        #:LEGAL
        (collect-values saturation 88 0 1 88 0.02 0.93))
      (test
        "4.03.14: (saturation 88 0 1 88 1 0)"
        #:LEGAL
        (collect-values saturation 88 0 1 88 1 0))
      (test
        "4.03.15: (saturation 74 1 0 75 0.95 0.0001)"
        #:LEGAL
        (collect-values saturation 74 1 0 75 0.95 0.0001))
      (test
        "4.03.16: (saturation 144 1.0 0.999 299 0.65 0.07)"
        #:LEGAL
        (collect-values saturation 144 1.0 0.999 299 0.65 0.07))
      (test
        "4.03.17: (saturation 243 0.0007 0.4 248 0.002 1.0)"
        #:LEGAL
        (collect-values saturation 243 0.0007 0.4 248 0.002 1.0)))
    (test-group "[4.04] value"
      (test
        "4.04.01: (value 0 0 0 0 0 0)"
        #:LEGAL
        (collect-values value 0 0 0 0 0 0))
      (test
        "4.04.02: (value 0 1 1 0 0 0)"
        #:LEGAL
        (collect-values value 0 1 1 0 0 0 ))
      (test
        "4.04.03: (value 0 1 1 0 1 1)"
        #:LEGAL
        (collect-values value 0 1 1 0 1 1))
      (test
        "4.04.04: (value 360 0 0 0 0 0)"
        #:LEGAL
        (collect-values value 360 0 0 0 0 0))
      (test
        "4.04.05: (value 360 1 1 360 1 1)"
        #:LEGAL
        (collect-values value 360 1 1 360 1 1))
      (test
        "4.04.06: (value 360 0 0 0 1 1)"
        #:LEGAL
        (collect-values value 360 0 0 0 1 1))
      (test
        "4.04.07: (value 360 1 1 360 0 0)"
        #:LEGAL
        (collect-values value 360 1 1 360 0 0))
      (test
        "4.04.08: (value 118 0.5 0.25 118 0.5 0.25)"
        #:LEGAL
        (collect-values value 118 0.5 0.25 118 0.5 0.25))
      (test
        "4.04.09: (value 118 0.5 0.25 99 1 1)"
        #:LEGAL
        (collect-values value 118 0.5 0.25 99 1 1))
      (test
        "4.04.10: (value 118 0.5 0.25 99 0 0)"
        #:LEGAL
        (collect-values value 118 0.5 0.25 99 0 0))
      (test
        "4.04.11: (value 271 0.01 0.002 44 1 0.007)"
        #:LEGAL
        (collect-values value 271 0.01 0.002 44 1 0.007))
      (test
        "4.04.12: (value 224 0.999 0.9 31 1 1)"
        #:LEGAL
        (collect-values value 224 0.999 0.9 31 1 1))
      (test
        "4.04.13: (value 88 0 1 88 0.02 0.93)"
        #:LEGAL
        (collect-values value 88 0 1 88 0.02 0.93))
      (test
        "4.04.14: (value 88 0 1 88 1 0)"
        #:LEGAL
        (collect-values value 88 0 1 88 1 0))
      (test
        "4.04.15: (value 74 1 0 75 0.95 0.0001)"
        #:LEGAL
        (collect-values value 74 1 0 75 0.95 0.0001))
      (test
        "4.04.16: (value 144 1.0 0.999 299 0.65 0.07)"
        #:LEGAL
        (collect-values value 144 1.0 0.999 299 0.65 0.07))
      (test
        "4.04.17: (value 243 0.0007 0.4 248 0.002 1.0)"
        #:LEGAL
        (collect-values value 243 0.0007 0.4 248 0.002 1.0)))
    ))
      

(test-exit)
