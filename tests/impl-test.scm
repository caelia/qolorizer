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

(current-test-epsilon 0.001)

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
        "2.01.09: (parse-color \"#fff\" #f) => '(1 1 1 1.0)"
        '(1 1 1 1.0)
        (collect-values parse-color "#fff" #f))
      (test
        "2.01.10: (parse-color \"#a7d733\" #f) => '(0.655 0.843 0.200 1.0)"
        '(0.655 0.843 0.200 1.0)
        (collect-values parse-color "#a7d733" #f))
      (with-comparator list-approx=
        (test
          "2.01.11: (parse-color \"#a7d73380\" #f) => '(0.655 0.843 0.200 0.5)"
          '(0.655 0.843 0.200 0.5)
          (collect-values parse-color "#a7d73380" #f))
        (test
          "2.01.12: (parse-color \"#a7d7334b\" #f) => '(0.655 0.843 0.200 0.294)"
          '(0.655 0.843 0.200 0.294)
          (collect-values parse-color "#a7d7334b" #f)))
      (test
        "2.01.13: (parse-color \"#6ac\" #f) => '(0.400 0.667 0.800 1.0)"
        '(0.400 0.667 0.800 1.0)
        (collect-values parse-color "#6ac" #f))
      (test
        "2.01.14: (parse-color \"#823f\" #f) => '(0.533 0.133 0.200 1.0)"
        '(0.533 0.133 0.200 1.0)
        (collect-values parse-color "#823f" #f))
      (test
        "2.01.15: (parse-color \"#823f\" 0.4) => '(0.533 0.133 0.200 0.4)"
        '(0.533 0.133 0.200 0.4)
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
        "2.02.10: (parse-color '(167 215 51) #f) => '(0.655 0.843 0.200 1.0)"
        '(0.655 0.843 0.200 1.0)
        (collect-values parse-color '(167 215 51) #f))
      (with-comparator list-approx=
        (test
          "2.02.11: (parse-color '(167 215 51 0.5) #f) => '(0.655 0.843 0.200 0.5)"
          '(0.655 0.843 0.200 0.5)
          (collect-values parse-color '(167 215 51 0.5) #f))
        (test
          "2.02.12: (parse-color '(167 215 51 0.294) #f) => '(0.655 0.843 0.200 0.294)"
          '(0.655 0.843 0.200 0.294)
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
        "2.03.10: (parse-color \"167,215,51\" #f) => '(0.655 0.843 0.200 1.0)"
        '(0.655 0.843 0.200 1.0)
        (collect-values parse-color "167,215,51" #f))
      (with-comparator list-approx=
        (test
          "2.03.11: (parse-color \"167,215,51,0.5\" #f) => '(0.655 0.843 0.200 0.5)"
          '(0.655 0.843 0.200 0.5)
          (collect-values parse-color "167,215,51,0.5" #f))
        (test
          "2.03.12: (parse-color \"167,215,51,0.294\" #f) => '(0.655 0.843 0.200 0.294)"
          '(0.655 0.843 0.200 0.294)
          (collect-values parse-color "167,215,51,0.294" #f)))
    )
))

(test-group "[3] RGB blend ops produce legal results w/ legal input"
  (with-comparator legal1
    (test-group "[3.01] normal"
      (test
        "3.01.01: ((normal-op 0) 0)"
        #:LEGAL
        ((normal-op 0) 0))
      (test
        "3.01.02: ((normal-op 0.067) 0.067)"
        #:LEGAL
        ((normal-op 0.067) 0.067))
      (test
        "3.01.03: ((normal-op 0.486) 0.486)"
        #:LEGAL
        ((normal-op 0.486) 0.486))
      (test
        "3.01.04: ((normal-op 1) 1)"
        #:LEGAL
        ((normal-op 1) 1))
      (test
        "3.01.05: ((normal-op 1) 0)"
        #:LEGAL
        ((normal-op 1) 0))
      (test
        "3.01.06: ((normal-op 0) 1)"
        #:LEGAL
        ((normal-op 0) 1))
      (test
        "3.01.07: ((normal-op 0) 0.004)"
        #:LEGAL
        ((normal-op 0) 0.004))
      (test
        "3.01.08: ((normal-op 0.004) 0)"
        #:LEGAL
        ((normal-op 0.004) 0))
      (test
        "3.01.09: ((normal-op 1) 0.996)"
        #:LEGAL
        ((normal-op 1) 0.996))
      (test
        "3.01.10: ((normal-op 0.996) 1)"
        #:LEGAL
        ((normal-op 0.996) 1))
      (test
        "3.01.11: ((normal-op 0.110) 0.114)"
        #:LEGAL
        ((normal-op 0.110) 0.114))
      (test
        "3.01.12: ((normal-op 0.114) 0.110)"
        #:LEGAL
        ((normal-op 0.114) 0.110))
      (test
        "3.01.13: ((normal-op 0.800) 0.067)"
        #:LEGAL
        ((normal-op 0.800) 0.067))
      (test
        "3.01.14: ((normal-op 0.067) 0.800)"
        #:LEGAL
        ((normal-op 0.067) 0.800))
      (test
        "3.01.15: ((normal-op 0.314) 0.231)"
        #:LEGAL
        ((normal-op 0.314) 0.231))
      (test
        "3.01.16: ((normal-op 0.231) 0.314)"
        #:LEGAL
        ((normal-op 0.231) 0.314)))
    (test-group "[3.02] dissolve"
      (test
        "3.02.01: ((dissolve-op 0) 0)"
        #:LEGAL
        ((dissolve-op 0) 0))
      (test
        "3.02.02: ((dissolve-op 0.067) 0.067)"
        #:LEGAL
        ((dissolve-op 0.067) 0.067))
      (test
        "3.02.03: ((dissolve-op 0.486) 0.486)"
        #:LEGAL
        ((dissolve-op 0.486) 0.486))
      (test
        "3.02.04: ((dissolve-op 1) 1)"
        #:LEGAL
        ((dissolve-op 1) 1))
      (test
        "3.02.05: ((dissolve-op 1) 0)"
        #:LEGAL
        ((dissolve-op 1) 0))
      (test
        "3.02.06: ((dissolve-op 0) 1)"
        #:LEGAL
        ((dissolve-op 0) 1))
      (test
        "3.02.07: ((dissolve-op 0) 0.004)"
        #:LEGAL
        ((dissolve-op 0) 0.004))
      (test
        "3.02.08: ((dissolve-op 0.004) 0)"
        #:LEGAL
        ((dissolve-op 0.004) 0))
      (test
        "3.02.09: ((dissolve-op 1) 0.996)"
        #:LEGAL
        ((dissolve-op 1) 0.996))
      (test
        "3.02.10: ((dissolve-op 0.996) 1)"
        #:LEGAL
        ((dissolve-op 0.996) 1))
      (test
        "3.02.11: ((dissolve-op 0.110) 0.114)"
        #:LEGAL
        ((dissolve-op 0.110) 0.114))
      (test
        "3.02.12: ((dissolve-op 0.114) 0.110)"
        #:LEGAL
        ((dissolve-op 0.114) 0.110))
      (test
        "3.02.13: ((dissolve-op 0.800) 0.067)"
        #:LEGAL
        ((dissolve-op 0.800) 0.067))
      (test
        "3.02.14: ((dissolve-op 0.067) 0.800)"
        #:LEGAL
        ((dissolve-op 0.067) 0.800))
      (test
        "3.02.15: ((dissolve-op 0.314) 0.231)"
        #:LEGAL
        ((dissolve-op 0.314) 0.231))
      (test
        "3.02.16: ((dissolve-op 0.231) 0.314)"
        #:LEGAL
        ((dissolve-op 0.231) 0.314)))
    (test-group "[3.03] multiply"
      (test
        "3.03.01: ((multiply-op 0) 0)"
        #:LEGAL
        ((multiply-op 0) 0))
      (test
        "3.03.02: ((multiply-op 0.067) 0.067)"
        #:LEGAL
        ((multiply-op 0.067) 0.067))
      (test
        "3.03.03: ((multiply-op 0.486) 0.486)"
        #:LEGAL
        ((multiply-op 0.486) 0.486))
      (test
        "3.03.04: ((multiply-op 1) 1)"
        #:LEGAL
        ((multiply-op 1) 1))
      (test
        "3.03.05: ((multiply-op 1) 0)"
        #:LEGAL
        ((multiply-op 1) 0))
      (test
        "3.03.06: ((multiply-op 0) 1)"
        #:LEGAL
        ((multiply-op 0) 1))
      (test
        "3.03.07: ((multiply-op 0) 0.004)"
        #:LEGAL
        ((multiply-op 0) 0.004))
      (test
        "3.03.08: ((multiply-op 0.004) 0)"
        #:LEGAL
        ((multiply-op 0.004) 0))
      (test
        "3.03.09: ((multiply-op 1) 0.996)"
        #:LEGAL
        ((multiply-op 1) 0.996))
      (test
        "3.03.10: ((multiply-op 0.996) 1)"
        #:LEGAL
        ((multiply-op 0.996) 1))
      (test
        "3.03.11: ((multiply-op 0.110) 0.114)"
        #:LEGAL
        ((multiply-op 0.110) 0.114))
      (test
        "3.03.12: ((multiply-op 0.114) 0.110)"
        #:LEGAL
        ((multiply-op 0.114) 0.110))
      (test
        "3.03.13: ((multiply-op 0.800) 0.067)"
        #:LEGAL
        ((multiply-op 0.800) 0.067))
      (test
        "3.03.14: ((multiply-op 0.067) 0.800)"
        #:LEGAL
        ((multiply-op 0.067) 0.800))
      (test
        "3.03.15: ((multiply-op 0.314) 0.231)"
        #:LEGAL
        ((multiply-op 0.314) 0.231))
      (test
        "3.03.16: ((multiply-op 0.231) 0.314)"
        #:LEGAL
        ((multiply-op 0.231) 0.314)))
    (test-group "[3.04] screen"
      (test
        "3.04.01: ((screen-op 0) 0)"
        #:LEGAL
        ((screen-op 0) 0))
      (test
        "3.04.02: ((screen-op 0.067) 0.067)"
        #:LEGAL
        ((screen-op 0.067) 0.067))
      (test
        "3.04.03: ((screen-op 0.486) 0.486)"
        #:LEGAL
        ((screen-op 0.486) 0.486))
      (test
        "3.04.04: ((screen-op 1) 1)"
        #:LEGAL
        ((screen-op 1) 1))
      (test
        "3.04.05: ((screen-op 1) 0)"
        #:LEGAL
        ((screen-op 1) 0))
      (test
        "3.04.06: ((screen-op 0) 1)"
        #:LEGAL
        ((screen-op 0) 1))
      (test
        "3.04.07: ((screen-op 0) 0.004)"
        #:LEGAL
        ((screen-op 0) 0.004))
      (test
        "3.04.08: ((screen-op 0.004) 0)"
        #:LEGAL
        ((screen-op 0.004) 0))
      (test
        "3.04.09: ((screen-op 1) 0.996)"
        #:LEGAL
        ((screen-op 1) 0.996))
      (test
        "3.04.10: ((screen-op 0.996) 1)"
        #:LEGAL
        ((screen-op 0.996) 1))
      (test
        "3.04.11: ((screen-op 0.110) 0.114)"
        #:LEGAL
        ((screen-op 0.110) 0.114))
      (test
        "3.04.12: ((screen-op 0.114) 0.110)"
        #:LEGAL
        ((screen-op 0.114) 0.110))
      (test
        "3.04.13: ((screen-op 0.800) 0.067)"
        #:LEGAL
        ((screen-op 0.800) 0.067))
      (test
        "3.04.14: ((screen-op 0.067) 0.800)"
        #:LEGAL
        ((screen-op 0.067) 0.800))
      (test
        "3.04.15: ((screen-op 0.314) 0.231)"
        #:LEGAL
        ((screen-op 0.314) 0.231))
      (test
        "3.04.16: ((screen-op 0.231) 0.314)"
        #:LEGAL
        ((screen-op 0.231) 0.314)))
    (test-group "[3.05] overlay"
      (test
        "3.05.01: ((overlay-op 0) 0)"
        #:LEGAL
        ((overlay-op 0) 0))
      (test
        "3.05.02: ((overlay-op 0.067) 0.067)"
        #:LEGAL
        ((overlay-op 0.067) 0.067))
      (test
        "3.05.03: ((overlay-op 0.486) 0.486)"
        #:LEGAL
        ((overlay-op 0.486) 0.486))
      (test
        "3.05.04: ((overlay-op 1) 1)"
        #:LEGAL
        ((overlay-op 1) 1))
      (test
        "3.05.05: ((overlay-op 1) 0)"
        #:LEGAL
        ((overlay-op 1) 0))
      (test
        "3.05.06: ((overlay-op 0) 1)"
        #:LEGAL
        ((overlay-op 0) 1))
      (test
        "3.05.07: ((overlay-op 0) 0.004)"
        #:LEGAL
        ((overlay-op 0) 0.004))
      (test
        "3.05.08: ((overlay-op 0.004) 0)"
        #:LEGAL
        ((overlay-op 0.004) 0))
      (test
        "3.05.09: ((overlay-op 1) 0.996)"
        #:LEGAL
        ((overlay-op 1) 0.996))
      (test
        "3.05.10: ((overlay-op 0.996) 1)"
        #:LEGAL
        ((overlay-op 0.996) 1))
      (test
        "3.05.11: ((overlay-op 0.110) 0.114)"
        #:LEGAL
        ((overlay-op 0.110) 0.114))
      (test
        "3.05.12: ((overlay-op 0.114) 0.110)"
        #:LEGAL
        ((overlay-op 0.114) 0.110))
      (test
        "3.05.13: ((overlay-op 0.800) 0.067)"
        #:LEGAL
        ((overlay-op 0.800) 0.067))
      (test
        "3.05.14: ((overlay-op 0.067) 0.800)"
        #:LEGAL
        ((overlay-op 0.067) 0.800))
      (test
        "3.05.15: ((overlay-op 0.314) 0.231)"
        #:LEGAL
        ((overlay-op 0.314) 0.231))
      (test
        "3.05.16: ((overlay-op 0.231) 0.314)"
        #:LEGAL
        ((overlay-op 0.231) 0.314)))
    (test-group "[3.06] difference"
      (test
        "3.06.01: ((difference-op 0) 0)"
        #:LEGAL
        ((difference-op 0) 0))
      (test
        "3.06.02: ((difference-op 0.067) 0.067)"
        #:LEGAL
        ((difference-op 0.067) 0.067))
      (test
        "3.06.03: ((difference-op 0.486) 0.486)"
        #:LEGAL
        ((difference-op 0.486) 0.486))
      (test
        "3.06.04: ((difference-op 1) 1)"
        #:LEGAL
        ((difference-op 1) 1))
      (test
        "3.06.05: ((difference-op 1) 0)"
        #:LEGAL
        ((difference-op 1) 0))
      (test
        "3.06.06: ((difference-op 0) 1)"
        #:LEGAL
        ((difference-op 0) 1))
      (test
        "3.06.07: ((difference-op 0) 0.004)"
        #:LEGAL
        ((difference-op 0) 0.004))
      (test
        "3.06.08: ((difference-op 0.004) 0)"
        #:LEGAL
        ((difference-op 0.004) 0))
      (test
        "3.06.09: ((difference-op 1) 0.996)"
        #:LEGAL
        ((difference-op 1) 0.996))
      (test
        "3.06.10: ((difference-op 0.996) 1)"
        #:LEGAL
        ((difference-op 0.996) 1))
      (test
        "3.06.11: ((difference-op 0.110) 0.114)"
        #:LEGAL
        ((difference-op 0.110) 0.114))
      (test
        "3.06.12: ((difference-op 0.114) 0.110)"
        #:LEGAL
        ((difference-op 0.114) 0.110))
      (test
        "3.06.13: ((difference-op 0.800) 0.067)"
        #:LEGAL
        ((difference-op 0.800) 0.067))
      (test
        "3.06.14: ((difference-op 0.067) 0.800)"
        #:LEGAL
        ((difference-op 0.067) 0.800))
      (test
        "3.06.15: ((difference-op 0.314) 0.231)"
        #:LEGAL
        ((difference-op 0.314) 0.231))
      (test
        "3.06.16: ((difference-op 0.231) 0.314)"
        #:LEGAL
        ((difference-op 0.231) 0.314)))
    (test-group "[3.07] addition"
      (test
        "3.07.01: ((addition-op 0) 0)"
        #:LEGAL
        ((addition-op 0) 0))
      (test
        "3.07.02: ((addition-op 0.067) 0.067)"
        #:LEGAL
        ((addition-op 0.067) 0.067))
      (test
        "3.07.03: ((addition-op 0.486) 0.486)"
        #:LEGAL
        ((addition-op 0.486) 0.486))
      (test
        "3.07.04: ((addition-op 1) 1)"
        #:LEGAL
        ((addition-op 1) 1))
      (test
        "3.07.05: ((addition-op 1) 0)"
        #:LEGAL
        ((addition-op 1) 0))
      (test
        "3.07.06: ((addition-op 0) 1)"
        #:LEGAL
        ((addition-op 0) 1))
      (test
        "3.07.07: ((addition-op 0) 0.004)"
        #:LEGAL
        ((addition-op 0) 0.004))
      (test
        "3.07.08: ((addition-op 0.004) 0)"
        #:LEGAL
        ((addition-op 0.004) 0))
      (test
        "3.07.09: ((addition-op 1) 0.996)"
        #:LEGAL
        ((addition-op 1) 0.996))
      (test
        "3.07.10: ((addition-op 0.996) 1)"
        #:LEGAL
        ((addition-op 0.996) 1))
      (test
        "3.07.11: ((addition-op 0.110) 0.114)"
        #:LEGAL
        ((addition-op 0.110) 0.114))
      (test
        "3.07.12: ((addition-op 0.114) 0.110)"
        #:LEGAL
        ((addition-op 0.114) 0.110))
      (test
        "3.07.13: ((addition-op 0.800) 0.067)"
        #:LEGAL
        ((addition-op 0.800) 0.067))
      (test
        "3.07.14: ((addition-op 0.067) 0.800)"
        #:LEGAL
        ((addition-op 0.067) 0.800))
      (test
        "3.07.15: ((addition-op 0.314) 0.231)"
        #:LEGAL
        ((addition-op 0.314) 0.231))
      (test
        "3.07.16: ((addition-op 0.231) 0.314)"
        #:LEGAL
        ((addition-op 0.231) 0.314)))
    (test-group "[3.08] subtract"
      (test
        "3.08.01: ((subtract-op 0) 0)"
        #:LEGAL
        ((subtract-op 0) 0))
      (test
        "3.08.02: ((subtract-op 0.067) 0.067)"
        #:LEGAL
        ((subtract-op 0.067) 0.067))
      (test
        "3.08.03: ((subtract-op 0.486) 0.486)"
        #:LEGAL
        ((subtract-op 0.486) 0.486))
      (test
        "3.08.04: ((subtract-op 1) 1)"
        #:LEGAL
        ((subtract-op 1) 1))
      (test
        "3.08.05: ((subtract-op 1) 0)"
        #:LEGAL
        ((subtract-op 1) 0))
      (test
        "3.08.06: ((subtract-op 0) 1)"
        #:LEGAL
        ((subtract-op 0) 1))
      (test
        "3.08.07: ((subtract-op 0) 0.004)"
        #:LEGAL
        ((subtract-op 0) 0.004))
      (test
        "3.08.08: ((subtract-op 0.004) 0)"
        #:LEGAL
        ((subtract-op 0.004) 0))
      (test
        "3.08.09: ((subtract-op 1) 0.996)"
        #:LEGAL
        ((subtract-op 1) 0.996))
      (test
        "3.08.10: ((subtract-op 0.996) 1)"
        #:LEGAL
        ((subtract-op 0.996) 1))
      (test
        "3.08.11: ((subtract-op 0.110) 0.114)"
        #:LEGAL
        ((subtract-op 0.110) 0.114))
      (test
        "3.08.12: ((subtract-op 0.114) 0.110)"
        #:LEGAL
        ((subtract-op 0.114) 0.110))
      (test
        "3.08.13: ((subtract-op 0.800) 0.067)"
        #:LEGAL
        ((subtract-op 0.800) 0.067))
      (test
        "3.08.14: ((subtract-op 0.067) 0.800)"
        #:LEGAL
        ((subtract-op 0.067) 0.800))
      (test
        "3.08.15: ((subtract-op 0.314) 0.231)"
        #:LEGAL
        ((subtract-op 0.314) 0.231))
      (test
        "3.08.16: ((subtract-op 0.231) 0.314)"
        #:LEGAL
        ((subtract-op 0.231) 0.314)))
    (test-group "[3.09] darken-only"
      (test
        "3.09.01: ((darken-only-op 0) 0)"
        #:LEGAL
        ((darken-only-op 0) 0))
      (test
        "3.09.02: ((darken-only-op 0.067) 0.067)"
        #:LEGAL
        ((darken-only-op 0.067) 0.067))
      (test
        "3.09.03: ((darken-only-op 0.486) 0.486)"
        #:LEGAL
        ((darken-only-op 0.486) 0.486))
      (test
        "3.09.04: ((darken-only-op 1) 1)"
        #:LEGAL
        ((darken-only-op 1) 1))
      (test
        "3.09.05: ((darken-only-op 1) 0)"
        #:LEGAL
        ((darken-only-op 1) 0))
      (test
        "3.09.06: ((darken-only-op 0) 1)"
        #:LEGAL
        ((darken-only-op 0) 1))
      (test
        "3.09.07: ((darken-only-op 0) 0.004)"
        #:LEGAL
        ((darken-only-op 0) 0.004))
      (test
        "3.09.08: ((darken-only-op 0.004) 0)"
        #:LEGAL
        ((darken-only-op 0.004) 0))
      (test
        "3.09.09: ((darken-only-op 1) 0.996)"
        #:LEGAL
        ((darken-only-op 1) 0.996))
      (test
        "3.09.10: ((darken-only-op 0.996) 1)"
        #:LEGAL
        ((darken-only-op 0.996) 1))
      (test
        "3.09.11: ((darken-only-op 0.110) 0.114)"
        #:LEGAL
        ((darken-only-op 0.110) 0.114))
      (test
        "3.09.12: ((darken-only-op 0.114) 0.110)"
        #:LEGAL
        ((darken-only-op 0.114) 0.110))
      (test
        "3.09.13: ((darken-only-op 0.800) 0.067)"
        #:LEGAL
        ((darken-only-op 0.800) 0.067))
      (test
        "3.09.14: ((darken-only-op 0.067) 0.800)"
        #:LEGAL
        ((darken-only-op 0.067) 0.800))
      (test
        "3.09.15: ((darken-only-op 0.314) 0.231)"
        #:LEGAL
        ((darken-only-op 0.314) 0.231))
      (test
        "3.09.16: ((darken-only-op 0.231) 0.314)"
        #:LEGAL
        ((darken-only-op 0.231) 0.314)))
    (test-group "[3.10] lighten-only"
      (test
        "3.10.01: ((lighten-only-op 0) 0)"
        #:LEGAL
        ((lighten-only-op 0) 0))
      (test
        "3.10.02: ((lighten-only-op 0.067) 0.067)"
        #:LEGAL
        ((lighten-only-op 0.067) 0.067))
      (test
        "3.10.03: ((lighten-only-op 0.486) 0.486)"
        #:LEGAL
        ((lighten-only-op 0.486) 0.486))
      (test
        "3.10.04: ((lighten-only-op 1) 1)"
        #:LEGAL
        ((lighten-only-op 1) 1))
      (test
        "3.10.05: ((lighten-only-op 1) 0)"
        #:LEGAL
        ((lighten-only-op 1) 0))
      (test
        "3.10.06: ((lighten-only-op 0) 1)"
        #:LEGAL
        ((lighten-only-op 0) 1))
      (test
        "3.10.07: ((lighten-only-op 0) 0.004)"
        #:LEGAL
        ((lighten-only-op 0) 0.004))
      (test
        "3.10.08: ((lighten-only-op 0.004) 0)"
        #:LEGAL
        ((lighten-only-op 0.004) 0))
      (test
        "3.10.09: ((lighten-only-op 1) 0.996)"
        #:LEGAL
        ((lighten-only-op 1) 0.996))
      (test
        "3.10.10: ((lighten-only-op 0.996) 1)"
        #:LEGAL
        ((lighten-only-op 0.996) 1))
      (test
        "3.10.11: ((lighten-only-op 0.110) 0.114)"
        #:LEGAL
        ((lighten-only-op 0.110) 0.114))
      (test
        "3.10.12: ((lighten-only-op 0.114) 0.110)"
        #:LEGAL
        ((lighten-only-op 0.114) 0.110))
      (test
        "3.10.13: ((lighten-only-op 0.800) 0.067)"
        #:LEGAL
        ((lighten-only-op 0.800) 0.067))
      (test
        "3.10.14: ((lighten-only-op 0.067) 0.800)"
        #:LEGAL
        ((lighten-only-op 0.067) 0.800))
      (test
        "3.10.15: ((lighten-only-op 0.314) 0.231)"
        #:LEGAL
        ((lighten-only-op 0.314) 0.231))
      (test
        "3.10.16: ((lighten-only-op 0.231) 0.314)"
        #:LEGAL
        ((lighten-only-op 0.231) 0.314)))
    (test-group "[3.11] divide"
      (test
        "3.11.01: ((divide-op 0) 0)"
        #:LEGAL
        ((divide-op 0) 0))
      (test
        "3.11.02: ((divide-op 0.067) 0.067)"
        #:LEGAL
        ((divide-op 0.067) 0.067))
      (test
        "3.11.03: ((divide-op 0.486) 0.486)"
        #:LEGAL
        ((divide-op 0.486) 0.486))
      (test
        "3.11.04: ((divide-op 1) 1)"
        #:LEGAL
        ((divide-op 1) 1))
      (test
        "3.11.05: ((divide-op 1) 0)"
        #:LEGAL
        ((divide-op 1) 0))
      (test
        "3.11.06: ((divide-op 0) 1)"
        #:LEGAL
        ((divide-op 0) 1))
      (test
        "3.11.07: ((divide-op 0) 0.004)"
        #:LEGAL
        ((divide-op 0) 0.004))
      (test
        "3.11.08: ((divide-op 0.004) 0)"
        #:LEGAL
        ((divide-op 0.004) 0))
      (test
        "3.11.09: ((divide-op 1) 0.996)"
        #:LEGAL
        ((divide-op 1) 0.996))
      (test
        "3.11.10: ((divide-op 0.996) 1)"
        #:LEGAL
        ((divide-op 0.996) 1))
      (test
        "3.11.11: ((divide-op 0.110) 0.114)"
        #:LEGAL
        ((divide-op 0.110) 0.114))
      (test
        "3.11.12: ((divide-op 0.114) 0.110)"
        #:LEGAL
        ((divide-op 0.114) 0.110))
      (test
        "3.11.13: ((divide-op 0.800) 0.067)"
        #:LEGAL
        ((divide-op 0.800) 0.067))
      (test
        "3.11.14: ((divide-op 0.067) 0.800)"
        #:LEGAL
        ((divide-op 0.067) 0.800))
      (test
        "3.11.15: ((divide-op 0.314) 0.231)"
        #:LEGAL
        ((divide-op 0.314) 0.231))
      (test
        "3.11.16: ((divide-op 0.231) 0.314)"
        #:LEGAL
        ((divide-op 0.231) 0.314)))
    (test-group "[3.12] dodge"
      (test
        "3.12.01: ((dodge-op 0) 0)"
        #:LEGAL
        ((dodge-op 0) 0))
      (test
        "3.12.02: ((dodge-op 0.067) 0.067)"
        #:LEGAL
        ((dodge-op 0.067) 0.067))
      (test
        "3.12.03: ((dodge-op 0.486) 0.486)"
        #:LEGAL
        ((dodge-op 0.486) 0.486))
      (test
        "3.12.04: ((dodge-op 1) 1)"
        #:LEGAL
        ((dodge-op 1) 1))
      (test
        "3.12.05: ((dodge-op 1) 0)"
        #:LEGAL
        ((dodge-op 1) 0))
      (test
        "3.12.06: ((dodge-op 0) 1)"
        #:LEGAL
        ((dodge-op 0) 1))
      (test
        "3.12.07: ((dodge-op 0) 0.004)"
        #:LEGAL
        ((dodge-op 0) 0.004))
      (test
        "3.12.08: ((dodge-op 0.004) 0)"
        #:LEGAL
        ((dodge-op 0.004) 0))
      (test
        "3.12.09: ((dodge-op 1) 0.996)"
        #:LEGAL
        ((dodge-op 1) 0.996))
      (test
        "3.12.10: ((dodge-op 0.996) 1)"
        #:LEGAL
        ((dodge-op 0.996) 1))
      (test
        "3.12.11: ((dodge-op 0.110) 0.114)"
        #:LEGAL
        ((dodge-op 0.110) 0.114))
      (test
        "3.12.12: ((dodge-op 0.114) 0.110)"
        #:LEGAL
        ((dodge-op 0.114) 0.110))
      (test
        "3.12.13: ((dodge-op 0.800) 0.067)"
        #:LEGAL
        ((dodge-op 0.800) 0.067))
      (test
        "3.12.14: ((dodge-op 0.067) 0.800)"
        #:LEGAL
        ((dodge-op 0.067) 0.800))
      (test
        "3.12.15: ((dodge-op 0.314) 0.231)"
        #:LEGAL
        ((dodge-op 0.314) 0.231))
      (test
        "3.12.16: ((dodge-op 0.231) 0.314)"
        #:LEGAL
        ((dodge-op 0.231) 0.314)))
    (test-group "[3.13] burn"
      (test
        "3.13.01: ((burn-op 0) 0)"
        #:LEGAL
        ((burn-op 0) 0))
      (test
        "3.13.02: ((burn-op 0.067) 0.067)"
        #:LEGAL
        ((burn-op 0.067) 0.067))
      (test
        "3.13.03: ((burn-op 0.486) 0.486)"
        #:LEGAL
        ((burn-op 0.486) 0.486))
      (test
        "3.13.04: ((burn-op 1) 1)"
        #:LEGAL
        ((burn-op 1) 1))
      (test
        "3.13.05: ((burn-op 1) 0)"
        #:LEGAL
        ((burn-op 1) 0))
      (test
        "3.13.06: ((burn-op 0) 1)"
        #:LEGAL
        ((burn-op 0) 1))
      (test
        "3.13.07: ((burn-op 0) 0.004)"
        #:LEGAL
        ((burn-op 0) 0.004))
      (test
        "3.13.08: ((burn-op 0.004) 0)"
        #:LEGAL
        ((burn-op 0.004) 0))
      (test
        "3.13.09: ((burn-op 1) 0.996)"
        #:LEGAL
        ((burn-op 1) 0.996))
      (test
        "3.13.10: ((burn-op 0.996) 1)"
        #:LEGAL
        ((burn-op 0.996) 1))
      (test
        "3.13.11: ((burn-op 0.110) 0.114)"
        #:LEGAL
        ((burn-op 0.110) 0.114))
      (test
        "3.13.12: ((burn-op 0.114) 0.110)"
        #:LEGAL
        ((burn-op 0.114) 0.110))
      (test
        "3.13.13: ((burn-op 0.800) 0.067)"
        #:LEGAL
        ((burn-op 0.800) 0.067))
      (test
        "3.13.14: ((burn-op 0.067) 0.800)"
        #:LEGAL
        ((burn-op 0.067) 0.800))
      (test
        "3.13.15: ((burn-op 0.314) 0.231)"
        #:LEGAL
        ((burn-op 0.314) 0.231))
      (test
        "3.13.16: ((burn-op 0.231) 0.314)"
        #:LEGAL
        ((burn-op 0.231) 0.314)))
    (test-group "[3.14] hard-light"
      (test
        "3.14.01: ((hard-light-op 0) 0)"
        #:LEGAL
        ((hard-light-op 0) 0))
      (test
        "3.14.02: ((hard-light-op 0.067) 0.067)"
        #:LEGAL
        ((hard-light-op 0.067) 0.067))
      (test
        "3.14.03: ((hard-light-op 0.486) 0.486)"
        #:LEGAL
        ((hard-light-op 0.486) 0.486))
      (test
        "3.14.04: ((hard-light-op 1) 1)"
        #:LEGAL
        ((hard-light-op 1) 1))
      (test
        "3.14.05: ((hard-light-op 1) 0)"
        #:LEGAL
        ((hard-light-op 1) 0))
      (test
        "3.14.06: ((hard-light-op 0) 1)"
        #:LEGAL
        ((hard-light-op 0) 1))
      (test
        "3.14.07: ((hard-light-op 0) 0.004)"
        #:LEGAL
        ((hard-light-op 0) 0.004))
      (test
        "3.14.08: ((hard-light-op 0.004) 0)"
        #:LEGAL
        ((hard-light-op 0.004) 0))
      (test
        "3.14.09: ((hard-light-op 1) 0.996)"
        #:LEGAL
        ((hard-light-op 1) 0.996))
      (test
        "3.14.10: ((hard-light-op 0.996) 1)"
        #:LEGAL
        ((hard-light-op 0.996) 1))
      (test
        "3.14.11: ((hard-light-op 0.110) 0.114)"
        #:LEGAL
        ((hard-light-op 0.110) 0.114))
      (test
        "3.14.12: ((hard-light-op 0.114) 0.110)"
        #:LEGAL
        ((hard-light-op 0.114) 0.110))
      (test
        "3.14.13: ((hard-light-op 0.800) 0.067)"
        #:LEGAL
        ((hard-light-op 0.800) 0.067))
      (test
        "3.14.14: ((hard-light-op 0.067) 0.800)"
        #:LEGAL
        ((hard-light-op 0.067) 0.800))
      (test
        "3.14.15: ((hard-light-op 0.314) 0.231)"
        #:LEGAL
        ((hard-light-op 0.314) 0.231))
      (test
        "3.14.16: ((hard-light-op 0.231) 0.314)"
        #:LEGAL
        ((hard-light-op 0.231) 0.314)))
    (test-group "[3.15] soft-light"
      (test
        "3.15.01: ((soft-light-op 0) 0)"
        #:LEGAL
        ((soft-light-op 0) 0))
      (test
        "3.15.02: ((soft-light-op 0.067) 0.067)"
        #:LEGAL
        ((soft-light-op 0.067) 0.067))
      (test
        "3.15.03: ((soft-light-op 0.486) 0.486)"
        #:LEGAL
        ((soft-light-op 0.486) 0.486))
      (test
        "3.15.04: ((soft-light-op 1) 1)"
        #:LEGAL
        ((soft-light-op 1) 1))
      (test
        "3.15.05: ((soft-light-op 1) 0)"
        #:LEGAL
        ((soft-light-op 1) 0))
      (test
        "3.15.06: ((soft-light-op 0) 1)"
        #:LEGAL
        ((soft-light-op 0) 1))
      (test
        "3.15.07: ((soft-light-op 0) 0.004)"
        #:LEGAL
        ((soft-light-op 0) 0.004))
      (test
        "3.15.08: ((soft-light-op 0.004) 0)"
        #:LEGAL
        ((soft-light-op 0.004) 0))
      (test
        "3.15.09: ((soft-light-op 1) 0.996)"
        #:LEGAL
        ((soft-light-op 1) 0.996))
      (test
        "3.15.10: ((soft-light-op 0.996) 1)"
        #:LEGAL
        ((soft-light-op 0.996) 1))
      (test
        "3.15.11: ((soft-light-op 0.110) 0.114)"
        #:LEGAL
        ((soft-light-op 0.110) 0.114))
      (test
        "3.15.12: ((soft-light-op 0.114) 0.110)"
        #:LEGAL
        ((soft-light-op 0.114) 0.110))
      (test
        "3.15.13: ((soft-light-op 0.800) 0.067)"
        #:LEGAL
        ((soft-light-op 0.800) 0.067))
      (test
        "3.15.14: ((soft-light-op 0.067) 0.800)"
        #:LEGAL
        ((soft-light-op 0.067) 0.800))
      (test
        "3.15.15: ((soft-light-op 0.314) 0.231)"
        #:LEGAL
        ((soft-light-op 0.314) 0.231))
      (test
        "3.15.16: ((soft-light-op 0.231) 0.314)"
        #:LEGAL
        ((soft-light-op 0.231) 0.314)))
    (test-group "[3.16] grain-extract"
      (test
        "3.16.01: ((grain-extract-op 0) 0)"
        #:LEGAL
        ((grain-extract-op 0) 0))
      (test
        "3.16.02: ((grain-extract-op 0.067) 0.067)"
        #:LEGAL
        ((grain-extract-op 0.067) 0.067))
      (test
        "3.16.03: ((grain-extract-op 0.486) 0.486)"
        #:LEGAL
        ((grain-extract-op 0.486) 0.486))
      (test
        "3.16.04: ((grain-extract-op 1) 1)"
        #:LEGAL
        ((grain-extract-op 1) 1))
      (test
        "3.16.05: ((grain-extract-op 1) 0)"
        #:LEGAL
        ((grain-extract-op 1) 0))
      (test
        "3.16.06: ((grain-extract-op 0) 1)"
        #:LEGAL
        ((grain-extract-op 0) 1))
      (test
        "3.16.07: ((grain-extract-op 0) 0.004)"
        #:LEGAL
        ((grain-extract-op 0) 0.004))
      (test
        "3.16.08: ((grain-extract-op 0.004) 0)"
        #:LEGAL
        ((grain-extract-op 0.004) 0))
      (test
        "3.16.09: ((grain-extract-op 1) 0.996)"
        #:LEGAL
        ((grain-extract-op 1) 0.996))
      (test
        "3.16.10: ((grain-extract-op 0.996) 1)"
        #:LEGAL
        ((grain-extract-op 0.996) 1))
      (test
        "3.16.11: ((grain-extract-op 0.110) 0.114)"
        #:LEGAL
        ((grain-extract-op 0.110) 0.114))
      (test
        "3.16.12: ((grain-extract-op 0.114) 0.110)"
        #:LEGAL
        ((grain-extract-op 0.114) 0.110))
      (test
        "3.16.13: ((grain-extract-op 0.800) 0.067)"
        #:LEGAL
        ((grain-extract-op 0.800) 0.067))
      (test
        "3.16.14: ((grain-extract-op 0.067) 0.800)"
        #:LEGAL
        ((grain-extract-op 0.067) 0.800))
      (test
        "3.16.15: ((grain-extract-op 0.314) 0.231)"
        #:LEGAL
        ((grain-extract-op 0.314) 0.231))
      (test
        "3.16.16: ((grain-extract-op 0.231) 0.314)"
        #:LEGAL
        ((grain-extract-op 0.231) 0.314)))
    (test-group "[3.17] grain-merge"
      (test
        "3.17.01: ((grain-merge-op 0) 0)"
        #:LEGAL
        ((grain-merge-op 0) 0))
      (test
        "3.17.02: ((grain-merge-op 0.067) 0.067)"
        #:LEGAL
        ((grain-merge-op 0.067) 0.067))
      (test
        "3.17.03: ((grain-merge-op 0.486) 0.486)"
        #:LEGAL
        ((grain-merge-op 0.486) 0.486))
      (test
        "3.17.04: ((grain-merge-op 1) 1)"
        #:LEGAL
        ((grain-merge-op 1) 1))
      (test
        "3.17.05: ((grain-merge-op 1) 0)"
        #:LEGAL
        ((grain-merge-op 1) 0))
      (test
        "3.17.06: ((grain-merge-op 0) 1)"
        #:LEGAL
        ((grain-merge-op 0) 1))
      (test
        "3.17.07: ((grain-merge-op 0) 0.004)"
        #:LEGAL
        ((grain-merge-op 0) 0.004))
      (test
        "3.17.08: ((grain-merge-op 0.004) 0)"
        #:LEGAL
        ((grain-merge-op 0.004) 0))
      (test
        "3.17.09: ((grain-merge-op 1) 0.996)"
        #:LEGAL
        ((grain-merge-op 1) 0.996))
      (test
        "3.17.10: ((grain-merge-op 0.996) 1)"
        #:LEGAL
        ((grain-merge-op 0.996) 1))
      (test
        "3.17.11: ((grain-merge-op 0.110) 0.114)"
        #:LEGAL
        ((grain-merge-op 0.110) 0.114))
      (test
        "3.17.12: ((grain-merge-op 0.114) 0.110)"
        #:LEGAL
        ((grain-merge-op 0.114) 0.110))
      (test
        "3.17.13: ((grain-merge-op 0.800) 0.067)"
        #:LEGAL
        ((grain-merge-op 0.800) 0.067))
      (test
        "3.17.14: ((grain-merge-op 0.067) 0.800)"
        #:LEGAL
        ((grain-merge-op 0.067) 0.800))
      (test
        "3.17.15: ((grain-merge-op 0.314) 0.231)"
        #:LEGAL
        ((grain-merge-op 0.314) 0.231))
      (test
        "3.17.16: ((grain-merge-op 0.231) 0.314)"
        #:LEGAL
        ((grain-merge-op 0.231) 0.314)))
))

(test-group "[4] HSV blend ops produce legal results w/ legal input"
  (with-comparator legal-hsv
    (test-group "[4.01] color"
      (test
        "4.01.01: ((color-op 0 0 0) 0 0 0)"
        #:LEGAL
        (collect-values (color-op 0 0 0) 0 0 0))
      (test
        "4.01.02: ((color-op 0 0 0) 0 1 1)"
        #:LEGAL
        (collect-values (color-op 0 0 0) 0 1 1))
      (test
        "4.01.03: ((color-op 0 1 1) 0 1 1)"
        #:LEGAL
        (collect-values (color-op 0 1 1) 0 1 1))
      (test
        "4.01.04: ((color-op 0 0 0) 360 0 0)"
        #:LEGAL
        (collect-values (color-op 0 0 0) 360 0 0))
      (test
        "4.01.05: ((color-op 360 1 1) 360 1 1)"
        #:LEGAL
        (collect-values (color-op 360 1 1) 360 1 1))
      (test
        "4.01.06: ((color-op 0 1 1) 360 0 0)"
        #:LEGAL
        (collect-values (color-op 0 1 1) 360 0 0))
      (test
        "4.01.07: ((color-op 360 0 0) 360 1 1)"
        #:LEGAL
        (collect-values (color-op 360 0 0) 360 1 1))
      (test
        "4.01.08: ((color-op 118 0.5 0.25) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (color-op 118 0.5 0.25) 118 0.5 0.25))
      (test
        "4.01.09: ((color-op 99 1 1) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (color-op 99 1 1) 118 0.5 0.25))
      (test
        "4.01.10: ((color-op 99 0 0) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (color-op 99 0 0) 118 0.5 0.25))
      (test
        "4.01.11: ((color-op 44 1 0.007) 271 0.01 0.002)"
        #:LEGAL
        (collect-values (color-op 44 1 0.007) 271 0.01 0.002))
      (test
        "4.01.12: ((color-op 31 1 1) 224 0.999 0.9)"
        #:LEGAL
        (collect-values (color-op 31 1 1) 224 0.999 0.9))
      (test
        "4.01.13: ((color-op 88 0.02 0.93) 88 0 1)"
        #:LEGAL
        (collect-values (color-op 88 0.02 0.93) 88 0 1))
      (test
        "4.01.14: ((color-op 88 1 0) 88 0 1)"
        #:LEGAL
        (collect-values (color-op 88 1 0) 88 0 1))
      (test
        "4.01.15: ((color-op 75 0.95 0.0001) 74 1 0)"
        #:LEGAL
        (collect-values (color-op 75 0.95 0.0001) 74 1 0))
      (test
        "4.01.16: ((color-op 299 0.65 0.07) 144 1.0 0.999)"
        #:LEGAL
        (collect-values (color-op 299 0.65 0.07) 144 1.0 0.999))
      (test
        "4.01.17: ((color-op 248 0.002 1.0) 243 0.0007 0.4)"
        #:LEGAL
        (collect-values (color-op 248 0.002 1.0) 243 0.0007 0.4)))
    (test-group "[4.02] hue"
      (test
        "4.02.01: ((hue-op 0 0 0) 0 0 0)"
        #:LEGAL
        (collect-values (hue-op 0 0 0) 0 0 0))
      (test
        "4.02.02: ((hue-op 0 0 0) 0 1 1)"
        #:LEGAL
        (collect-values (hue-op 0 0 0) 0 1 1))
      (test
        "4.02.03: ((hue-op 0 1 1) 0 1 1)"
        #:LEGAL
        (collect-values (hue-op 0 1 1) 0 1 1))
      (test
        "4.02.04: ((hue-op 0 0 0) 360 0 0)"
        #:LEGAL
        (collect-values (hue-op 0 0 0) 360 0 0))
      (test
        "4.02.05: ((hue-op 360 1 1) 360 1 1)"
        #:LEGAL
        (collect-values (hue-op 360 1 1) 360 1 1))
      (test
        "4.02.06: ((hue-op 0 1 1) 360 0 0)"
        #:LEGAL
        (collect-values (hue-op 0 1 1) 360 0 0))
      (test
        "4.02.07: ((hue-op 360 0 0) 360 1 1)"
        #:LEGAL
        (collect-values (hue-op 360 0 0) 360 1 1))
      (test
        "4.02.08: ((hue-op 118 0.5 0.25) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (hue-op 118 0.5 0.25) 118 0.5 0.25))
      (test
        "4.02.09: ((hue-op 99 1 1) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (hue-op 99 1 1) 118 0.5 0.25))
      (test
        "4.02.10: ((hue-op 99 0 0) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (hue-op 99 0 0) 118 0.5 0.25))
      (test
        "4.02.11: ((hue-op 44 1 0.007) 271 0.01 0.002)"
        #:LEGAL
        (collect-values (hue-op 44 1 0.007) 271 0.01 0.002))
      (test
        "4.02.12: ((hue-op 31 1 1) 224 0.999 0.9)"
        #:LEGAL
        (collect-values (hue-op 31 1 1) 224 0.999 0.9))
      (test
        "4.02.13: ((hue-op 88 0.02 0.93) 88 0 1)"
        #:LEGAL
        (collect-values (hue-op 88 0.02 0.93) 88 0 1))
      (test
        "4.02.14: ((hue-op 88 1 0) 88 0 1)"
        #:LEGAL
        (collect-values (hue-op 88 1 0) 88 0 1))
      (test
        "4.02.15: ((hue-op 75 0.95 0.0001) 74 1 0)"
        #:LEGAL
        (collect-values (hue-op 75 0.95 0.0001) 74 1 0))
      (test
        "4.02.16: ((hue-op 299 0.65 0.07) 144 1.0 0.999)"
        #:LEGAL
        (collect-values (hue-op 299 0.65 0.07) 144 1.0 0.999))
      (test
        "4.02.17: ((hue-op 248 0.002 1.0) 243 0.0007 0.4)"
        #:LEGAL
        (collect-values (hue-op 248 0.002 1.0) 243 0.0007 0.4)))
    (test-group "[4.03] saturation"
      (test
        "4.03.01: ((saturation-op 0 0 0) 0 0 0)"
        #:LEGAL
        (collect-values (saturation-op 0 0 0) 0 0 0))
      (test
        "4.03.02: ((saturation-op 0 0 0) 0 1 1)"
        #:LEGAL
        (collect-values (saturation-op 0 0 0) 0 1 1))
      (test
        "4.03.03: ((saturation-op 0 1 1) 0 1 1)"
        #:LEGAL
        (collect-values (saturation-op 0 1 1) 0 1 1))
      (test
        "4.03.04: ((saturation-op 0 0 0) 360 0 0)"
        #:LEGAL
        (collect-values (saturation-op 0 0 0) 360 0 0))
      (test
        "4.03.05: ((saturation-op 360 1 1) 360 1 1)"
        #:LEGAL
        (collect-values (saturation-op 360 1 1) 360 1 1))
      (test
        "4.03.06: ((saturation-op 0 1 1) 360 0 0)"
        #:LEGAL
        (collect-values (saturation-op 0 1 1) 360 0 0))
      (test
        "4.03.07: ((saturation-op 360 0 0) 360 1 1)"
        #:LEGAL
        (collect-values (saturation-op 360 0 0) 360 1 1))
      (test
        "4.03.08: ((saturation-op 118 0.5 0.25) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (saturation-op 118 0.5 0.25) 118 0.5 0.25))
      (test
        "4.03.09: ((saturation-op 99 1 1) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (saturation-op 99 1 1) 118 0.5 0.25))
      (test
        "4.03.10: ((saturation-op 99 0 0) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (saturation-op 99 0 0) 118 0.5 0.25))
      (test
        "4.03.11: ((saturation-op 44 1 0.007) 271 0.01 0.002)"
        #:LEGAL
        (collect-values (saturation-op 44 1 0.007) 271 0.01 0.002))
      (test
        "4.03.12: ((saturation-op 31 1 1) 224 0.999 0.9)"
        #:LEGAL
        (collect-values (saturation-op 31 1 1) 224 0.999 0.9))
      (test
        "4.03.13: ((saturation-op 88 0.02 0.93) 88 0 1)"
        #:LEGAL
        (collect-values (saturation-op 88 0.02 0.93) 88 0 1))
      (test
        "4.03.14: ((saturation-op 88 1 0) 88 0 1)"
        #:LEGAL
        (collect-values (saturation-op 88 1 0) 88 0 1))
      (test
        "4.03.15: ((saturation-op 75 0.95 0.0001) 74 1 0)"
        #:LEGAL
        (collect-values (saturation-op 75 0.95 0.0001) 74 1 0))
      (test
        "4.03.16: ((saturation-op 299 0.65 0.07) 144 1.0 0.999)"
        #:LEGAL
        (collect-values (saturation-op 299 0.65 0.07) 144 1.0 0.999))
      (test
        "4.03.17: ((saturation-op 248 0.002 1.0) 243 0.0007 0.4)"
        #:LEGAL
        (collect-values (saturation-op 248 0.002 1.0) 243 0.0007 0.4)))
    (test-group "[4.04] value"
      (test
        "4.04.01: ((value-op 0 0 0) 0 0 0)"
        #:LEGAL
        (collect-values (value-op 0 0 0) 0 0 0))
      (test
        "4.04.02: ((value-op 0 0 0) 0 1 1)"
        #:LEGAL
        (collect-values (value-op 0 0 0) 0 1 1))
      (test
        "4.04.03: ((value-op 0 1 1) 0 1 1)"
        #:LEGAL
        (collect-values (value-op 0 1 1) 0 1 1))
      (test
        "4.04.04: ((value-op 0 0 0) 360 0 0)"
        #:LEGAL
        (collect-values (value-op 0 0 0) 360 0 0))
      (test
        "4.04.05: ((value-op 360 1 1) 360 1 1)"
        #:LEGAL
        (collect-values (value-op 360 1 1) 360 1 1))
      (test
        "4.04.06: ((value-op 0 1 1) 360 0 0)"
        #:LEGAL
        (collect-values (value-op 0 1 1) 360 0 0))
      (test
        "4.04.07: ((value-op 360 0 0) 360 1 1)"
        #:LEGAL
        (collect-values (value-op 360 0 0) 360 1 1))
      (test
        "4.04.08: ((value-op 118 0.5 0.25) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (value-op 118 0.5 0.25) 118 0.5 0.25))
      (test
        "4.04.09: ((value-op 99 1 1) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (value-op 99 1 1) 118 0.5 0.25))
      (test
        "4.04.10: ((value-op 99 0 0) 118 0.5 0.25)"
        #:LEGAL
        (collect-values (value-op 99 0 0) 118 0.5 0.25))
      (test
        "4.04.11: ((value-op 44 1 0.007) 271 0.01 0.002)"
        #:LEGAL
        (collect-values (value-op 44 1 0.007) 271 0.01 0.002))
      (test
        "4.04.12: ((value-op 31 1 1) 224 0.999 0.9)"
        #:LEGAL
        (collect-values (value-op 31 1 1) 224 0.999 0.9))
      (test
        "4.04.13: ((value-op 88 0.02 0.93) 88 0 1)"
        #:LEGAL
        (collect-values (value-op 88 0.02 0.93) 88 0 1))
      (test
        "4.04.14: ((value-op 88 1 0) 88 0 1)"
        #:LEGAL
        (collect-values (value-op 88 1 0) 88 0 1))
      (test
        "4.04.15: ((value-op 75 0.95 0.0001) 74 1 0)"
        #:LEGAL
        (collect-values (value-op 75 0.95 0.0001) 74 1 0))
      (test
        "4.04.16: ((value-op 299 0.65 0.07) 144 1.0 0.999)"
        #:LEGAL
        (collect-values (value-op 299 0.65 0.07) 144 1.0 0.999))
      (test
        "4.04.17: ((value-op 248 0.002 1.0) 243 0.0007 0.4)"
        #:LEGAL
        (collect-values (value-op 248 0.002 1.0) 243 0.0007 0.4)))
    ))
      

(test-exit)
