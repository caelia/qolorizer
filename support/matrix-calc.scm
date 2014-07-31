(include "alpha-matrix.scm")

(define (mk-lookup-table color)
  (let-values (((u-index l-index r-values) (get-matrix color)))
    (lambda (u l)
      (let ((uidx (list-index (lambda (elt) (= u elt)) u-index))
            (lidx (list-index (lambda (elt) (= l elt)) l-index)))
        (and uidx lidx (list-index (list-index r-values uidx) lidx))))))

(define (fcomp f tbl u l)
  (let ((comp-result (f u l))
        (tbl-result (tbl u l)))
    (if (and comp-result tbl-result (= comp-result tbl-result))
      "OK"
      (sprintf "Calculated: ~A | Table: ~A" comp-result tbl-result))))
