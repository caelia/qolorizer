(include "alpha-matrix.scm")

(define (print-row row-data)
  (for-each
    (lambda (datum) (display ",") (display datum))
    row-data)
  (newline))

(define (print-data-row row)
  (let ((row-label (car row))
        (row-data (cadr row)))
    (display row-label)
    (print-row row-data)))
  
(define (print-matrix color)
  (let-values (((u-alphas l-alphas r-alphas) (get-matrix color)))
    (with-output-to-file
      (string-append "mtx-" color ".csv")
      (lambda ()
        (print-row u-alphas)
        (for-each print-data-row (zip l-alphas r-alphas))))))
            
(define colors '("white" "grey" "black"))

(for-each print-matrix colors)
