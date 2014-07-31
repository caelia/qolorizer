(use imlib2)
(use srfi-1)
(use srfi-69)

(define (pixel-variance ref-img test-img x y)
  (let-values (((ref-r ref-g ref-b ref-a) (image-pixel/rgba ref-img x y))
               ((test-r test-g test-b test-a) (image-pixel/rgba test-img x y)))
    (let ((v (lambda (r t) (abs (- r t)))))
      (list (v ref-r test-r) (v ref-g test-g) (v ref-b test-b) (v ref-a test-a)))))

(define (img-variance ref-img test-img)
  (let ((width (image-width ref-img))
        (height (image-height ref-img))
        (test-width (image-width test-img))
        (test-height (image-height test-img)))
    (assert (and (= test-width width) (= test-height height)))
    (let loop ((x 4) (y 2) (rows '()) (cells '()))
      (cond
        ((>= y height) (reverse rows))
        ((>= x width) (loop 4 (+ y 4) (cons (reverse cells) rows) '()))
        (else (loop (+ x 8) y rows (cons (pixel-variance ref-img test-img x y) cells)))))))

(define (sequence-summary prev cell)
  (let ((cell-r (first cell))
        (cell-g (second cell))
        (cell-b (third cell))
        (cell-a (fourth cell)))
    (let ((r (+ cell-r (first prev)))
          (g (+ cell-g (second prev)))
          (b (+ cell-b (second prev)))
          (a (+ cell-a (second prev))))
      (list r g b a (+ (/ (+ cell-r cell-g cell-b cell-a) 4) (fifth prev))))))

(define (row-mean row)
  (map
    (lambda (sum) (/ sum (length row)))
    (foldl
      sequence-summary
      '(0 0 0 0 0)
      row)))

(define (col-mean rows idx)
  (map
    (lambda (sum) (/ sum (length rows)))
    (foldl
      sequence-summary
      '(0 0 0 0 0)
      (foldl
        (lambda (col row)
          (cons (list-ref row idx) col))
          '()
          rows))))

(define (crosscheck rows cols)
  (let ((list=
          (lambda (l1 l2) (every = l1 l2) l1))
        (seq-avgs
          (lambda (seq)
            (map
              (lambda (x) (/ x (length seq)))
              (foldl
                (lambda (prev rgbac)
                  (let ((r (+ (first prev) (first rgbac)))
                        (g (+ (second prev) (second rgbac)))
                        (b (+ (third prev) (third rgbac)))
                        (a (+ (fourth prev) (fourth rgbac)))
                        (c (+ (fifth prev) (fifth rgbac))))
                    (list r g b a c)))
                '(0 0 0 0 0)
                seq)))))
    (or (list= (seq-avgs rows) (seq-avgs cols))
        (error "Row & column averages don't match."))))

(define (img-summary img-vdata)
  (let ((rows (map row-mean img-vdata))
        (cols (map (lambda (i) (col-mean img-vdata i)) (iota (length (car img-vdata))))))
    (let ((all-avgs (crosscheck rows cols)))
      (list all-avgs rows cols))))

(define all-data (make-hash-table))

(define (test-image-pair ref-path test-path blend color alpha)
  (let* ((ref-file (foldl make-pathname ref-path `(,blend ,color ,alpha "colors.png")))
         (test-file (foldl make-pathname test-path `(,blend ,color ,alpha "colors.png")))
         (ref-img (image-load ref-file))
         (test-img (image-load test-file))
         (vdata (img-variance ref-img test-img)))
    (image-destroy ref-img)
    (image-destroy test-img)
    `(,@(img-summary vdata) ,vdata)))

(define (test-and-store ref-path test-path blend color alpha)
  (let ((key (list blend color alpha))
        (data (test-image-pair ref-path test-path blend color alpha)))
    (hash-table-set! all-data key data)))

(define blend-modes
  '("addition" "burn" "color" "darken_only" "difference"
    "divide" "dodge" "grain_extract" "grain_merge" "hardlight"
    "hue" "lighten_only" "multiply" "normal" "saturation"
    "screen" "subtract" "value"))

(define colors
  '("000000" "0000ff" "00ff00" "27249c" "54fa0d" "5c5c5c" "775acf" "7f7d0a"
    "808080" "899675" "98d5e4" "b5b5b5" "cd1f3c" "f62db6" "ff0000" "ffffff"))

(define alpha-levels
  '("0.2" "0.43" "0.6" "0.78" "1.0"))

(define (test-and-store-all ref-path test-path)
  (let ((count 0))
    (for-each
      (lambda (mode)
        (for-each
          (lambda (color)
            (for-each
              (lambda (alpha)
                (test-and-store ref-path test-path mode color alpha)
                (set! count (+ count 1))
                (when (= (modulo count 20) 0)
                  (printf " . . . ~A" count)
                  (flush-output)))
              alpha-levels))
          colors))
      blend-modes))
  (newline))

(define (summary #!key (mode #f) (color #f) (alpha #f))
  (let* ((filtr
          (lambda (key)
            (cond
              (mode (equal? (car key) mode))
              (color (equal? (cadr key) color))
              (alpha (equal? (caddr key) alpha))
              (else #t))))
          (keys
            (filter filtr (hash-table-keys all-data)))
          (len
            (length keys)))
    (map 
      (lambda (x) (/ x len))
      (foldl
        (lambda (prev k)
          (let ((data (car (hash-table-ref all-data k))))
            (let ((r (+ (first prev) (first data)))
                  (g (+ (second prev) (second data)))
                  (b (+ (third prev) (third data)))
                  (a (+ (fourth prev) (fourth data)))
                  (c (+ (fifth prev) (fifth data))))
              (list r g b a c))))
        '(0 0 0 0 0)
        keys))))

(define (pad s len chr right)
  (let ((padstr (make-string (max (- len (string-length s)) 0) chr)))
    (if right
      (string-append s padstr)
      (string-append padstr s))))

(define (lpad s len #!optional (chr #\space))
  (pad s len chr #f))

(define (rpad s len #!optional (chr #\space))
  (pad s len chr #t))

(define (format-number n llen precision)
  (if (fixnum? n)
    (let* ((s (number->string n))
           (slen (string-length s))
           (rspace (make-string (+ precision 1) #\space))
           (lspace (if (>= slen llen) "" (make-string (- llen slen) #\space))))
      (string-append lspace s rspace))
    (let* ((mult (expt 10 precision))
           (f (/ (inexact->exact (round (* n mult))) mult))
           (parts (string-split (number->string f) "." #t))
           (sint (car parts))
           (sfrac (if (null? (cdr parts)) "0" (cadr parts)))
           (lint (string-length sint))
           (lfrac (string-length sfrac))
           (lspace (if (>= lint llen) "" (make-string (- llen lint) #\space)))
           (rspace (make-string (- precision lfrac) #\space)))
      (string-append lspace sint "." sfrac rspace))))

(define (write-image-header mode color alpha)
  (let ((text (string-append mode "/" color "/" alpha " ")))
    (print (rpad text 80 #\:)))
  (newline))

(define (format-data-set raw)
  (let* ((data
         (map
           (lambda (n) (format-number n 3 3))
           raw))
         (r (first data))
         (g (second data))
         (b (third data))
         (a (fourth data))
         (c (fifth data)))
    (sprintf "R ~A  G ~A  B ~A  A ~A  Comp ~A" r g b a c)))

(define (write-image-mean data)
  (let ((main-text (format-data-set data)))
    (display "MEAN VALUES FOR IMAGE: ")
    (print main-text)
    (newline)))

(define (write-seq-data data #!key (first #f) (is-col #f))
  (let ((main-text (format-data-set data)))
    (cond
      ((and first is-col) (display "        Column values: "))
      (first (display "           Row values: "))
      (else (display "                       ")))
    (print main-text)))

(define (write-all-image-summary filename) 
  (let* ((sfun
          (lambda (a b)
            (let ((a-one (car a))
                  (b-one (car b))
                  (a-two (cadr a))
                  (b-two (cadr b)))
              (or (string<? a-one b-one)
                  (and (string=? a-one b-one) (string<? a-two b-two))
                  (and (string=? a-one b-one) (string=? a-two b-two) (string<? (caddr a) (caddr b)))))))
         (keys
           (sort (hash-table-keys all-data) sfun)))
    (with-output-to-file
      filename
      (lambda ()
        (for-each
          (lambda (key)
            (let ((data (hash-table-ref all-data key)))
              (apply write-image-header key)
              (let ((all (car data))
                    (rows (cadr data))
                    (cols (caddr data)))
                (write-image-mean all)
                (write-seq-data (car rows) first: #t)
                (for-each write-seq-data (cdr rows))
                (newline)
                (write-seq-data (car cols) first: #t is-col: #t)
                (for-each
                  (lambda (c) (write-seq-data c is-col: #t))
                  (cdr cols))
                (newline)
                (newline))))
          keys)))))

(define (test-and-report ref-path test-path report-file #!optional (force #f))
  (when (and (file-exists? report-file) (not force))
    (error (sprintf "File exists: ~A" report-file)))
  (hash-table-clear! all-data)
  (test-and-store-all ref-path test-path)
  (write-all-image-summary report-file))
