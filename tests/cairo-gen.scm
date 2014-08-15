(use qolorizer)
(import extras)

(define blend-modes
  '(multiply screen overlay darken-only lighten-only
    dodge burn hard-light soft-light difference
    exclusion hue saturation color luminosity))

(define colors
  '("000000" "0000ff" "00ff00" "27249c" "54fa0d" "5c5c5c" "775acf" "7f7d0a"
    "808080" "899675" "98d5e4" "b5b5b5" "cd1f3c" "f62db6" "ff0000" "ffffff"))

(define alpha-levels
  '("0.2" "0.43" "0.6" "0.78" "1.0"))

(define (mk-img-path dir blend color alpha #!optional (base "colors.png"))
  (foldl make-pathname dir `(,blend ,color ,alpha ,base)))

(define (create-test-image src-file mode color alpha)
  (let* ((dest-file (mk-img-path "images/cairo-test" (symbol->string mode) color alpha))
         (cspec (string-append "#" color)) 
         (alpha* (string->number alpha))
         (blend (mk-blend-op cspec blend-mode: mode alpha: alpha*)))
    (create-directory (pathname-directory dest-file) #t)
    (colorize blend src-file dest-file)))

(define (create-test-images) 
  (printf ":::: Generating ~A test images ... this may take a few minutes! ::::::::::::\n"
          (* (length colors) (length blend-modes) (length alpha-levels)))
  (let ((count 0)
        (src "images/base.png"))
    (for-each
      (lambda (mode)
        (for-each
          (lambda (color)
            (for-each
              (lambda (alpha)
                (create-test-image src mode color alpha)
                (set! count (+ count 1))
                (when (= (modulo count 10) 0)
                  (display " :"))
                (when (= (modulo count 100) 0)
                  (printf " ~A" count)
                  (flush-output)))
              alpha-levels))
          colors))
        blend-modes)
    (print ":::: DONE ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")))

(create-test-images)
