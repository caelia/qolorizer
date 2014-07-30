(use srfi-1)

(define CORRECT (make-parameter 0))
(define MINOR_ERROR (make-parameter 1))
(define MAJOR_ERROR (make-parameter 2))

(define (zpad n len)
  (let ((ns (number->string n)))
    (let loop ((s ns))
      (if (>= (string-length s) len)
        s
        (loop (string-append " " s))))))

(define (header-row colors)
  (apply printf `("         ~A        ~A        ~A        ~A        ~A\n" ,@colors)))

(define (footer-row colors)
  (apply printf `("      ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A\n" ,@colors)))

(define (cell scores #!optional (wide #f))
  (let* ((repr
          (lambda (score)
            (cond
              ((= score (CORRECT)) #\-)
              ((= score (MINOR_ERROR)) #\:)
              ((= score (MAJOR_ERROR)) #\!))))
         (pre-space
           (if wide "   " ""))
         (post-space
           (if wide "    " "")))
    (apply printf `(" [~A~A~A~A~A~A]" ,pre-space ,@(map repr scores) ,post-space))))

(define (row-label lbl)
  (display (zpad lbl 5)))

(define (row label scores #!optional (wide #f))
  (row-label label)
  (for-each
    (lambda (s) (cell s wide))
    scores)
  (newline))

(define (block head-colors foot-colors alphas scores)
  (let* ((top-half (zip alphas (take scores (length alphas))))
         (bottom-half (zip (reverse alphas) (drop scores (length alphas)))))
    (header-row head-colors)
    (for-each
      (lambda (data) (row (car data) (cadr data) #t))
      top-half)
    (for-each
      (lambda (data) (row (car data) (cadr data)))
      bottom-half)
    (footer-row foot-colors)))
    
(define sample-headcolors '("ffffff" "bfbfbf" "808080" "404040" "000000"))
(define sample-footcolors '("ff0000" "cc3434" "cccc00" "a2a329" "00ff00"
                            "34cc34" "00cccc" "29a2a3" "0000ff" "3434cc"))
(define sample-alphas '(0 25 50 74 100))
(define sample-scores
  '(((0 0 0 2) (0 0 0 2) (0 1 0 2) (0 0 0 0) (0 1 0 0))
    ((0 0 0 0) (0 0 0 0) (1 0 0 1) (0 1 1 0) (0 0 0 0))
    ((0 0 1 0) (0 0 0 0) (0 1 0 0) (0 0 0 1) (0 0 0 2))
    ((1 0 0 0) (1 1 1 0) (0 0 0 0) (0 0 0 0) (0 2 0 0))
    ((2 1 0 1) (2 1 0 0) (2 0 0 0) (0 0 1 0) (0 0 0 0))
    ((0 0 0 0) (0 0 0 0) (0 1 0 0) (0 0 0 0) (0 0 0 0)
     (0 0 0 2) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
    ((0 0 0 1) (1 0 1 0) (0 0 0 0) (0 0 0 0) (0 2 0 0)
     (0 2 0 0) (0 0 0 2) (0 0 0 0) (0 1 0 0) (0 0 0 0))
    ((1 0 1 0) (0 0 1 0) (0 0 0 0) (1 0 0 1) (1 0 0 0)
     (0 0 0 0) (0 0 0 2) (0 0 0 0) (0 0 0 0) (0 0 0 0))
    ((0 0 1 0) (0 0 0 0) (0 1 0 0) (0 0 0 1) (0 0 0 0)
     (1 0 0 0) (0 0 0 0) (0 0 0 0) (0 1 1 2) (0 1 0 0))
    ((0 1 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 1 0 0)
     (1 0 0 1) (0 0 1 0) (0 0 0 0) (0 0 0 0) (0 0 0 2))))

;; FORMAT EXAMPLE

;        ffffff        bfbfbf        808080        404040        000000
;   0 [   ----    ] [   ----    ] [   ----    ] [   ----    ] [   ----    ]
;  25 [   ----    ] [   ----    ] [   ----    ] [   ----    ] [   ----    ]
;  50 [   ----    ] [   ----    ] [   ----    ] [   ----    ] [   ----    ]
;  74 [   ----    ] [   ----    ] [   ----    ] [   ----    ] [   ----    ]
; 100 [   ----    ] [   ----    ] [   ----    ] [   ----    ] [   ----    ]
; 100 [----] [----] [----] [----] [----] [----] [----] [----] [----] [----]
;  74 [----] [----] [----] [----] [----] [----] [----] [----] [----] [----]
;  50 [----] [----] [----] [----] [----] [----] [----] [----] [----] [----]
;  25 [----] [----] [----] [----] [----] [----] [----] [----] [----] [----]
;   0 [----] [----] [----] [----] [----] [----] [----] [----] [----] [----]
;     ff0000 cc3434 cccc00 a2a329 00ff00 34cc34 00cccc 29a2a3 0000ff 3434cc
