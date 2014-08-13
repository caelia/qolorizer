(use imlib2)
(use srfi-1)

(define (get-pixel img x y)
  (let-values (((r g b a) (image-pixel/rgba img x y)))
    (list r g b a)))

(define (get-image-pixels filename)
  (let* ((img (image-load filename))
         (width (image-width img))
         (height (image-height img)))
    (let ((pixel-data
          (let loop ((x 4) (y 2) (pixels '()) (rows '()))
            (cond
              ((> y height) (reverse rows))
              ((> x width) (loop 4 (+ y 4) '() (cons (reverse pixels) rows)))
              (else (loop (+ x 8) y (cons (get-pixel img x y) pixels) rows))))))
    (image-destroy img)
    pixel-data)))

(define (get-ref-file-pixels ref-path blend color alpha)
  (let ((ref-file (foldl make-pathname ref-path `(,blend ,color ,alpha "colors.png"))))
    (get-image-pixels ref-file)))

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

(define (get-ref-data ref-path)
  (map
    (lambda (mode)
      (cons
        mode
        (map
          (lambda (color)
            (cons
              color
              (map
                (lambda (alpha)
                  (cons alpha (get-ref-file-pixels ref-path mode color alpha)))
                alpha-levels)))
          colors)))
    blend-modes))

(define (create-report base-file ref-path outfile)
  (let* ((base-data (get-image-pixels base-file))
         (ref-data (get-ref-data ref-path))
         (all-data (list (cons 'base base-data) (cons 'ref-images ref-data))))
    (with-output-to-file outfile (lambda () (write all-data)))))
