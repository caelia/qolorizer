(use test)
(use imlib2)
(use qolorizer)
(use files)
(use posix)
(include "unpack.scm")

(define base-image "images/base.png")
(define ref-base-path "images/ref")
(define test-base-path "images/test")
(define test-filename "colors.png")

(define colors
  '("000000" "0000ff" "00ff00" "27249c" "54fa0d" "5c5c5c" "775acf" "7f7d0a"
    "808080" "899675" "98d5e4" "b5b5b5" "cd1f3c" "f62db6" "ff0000" "ffffff"))

(define testable-blend-modes
  '(addition burn color darken-only difference divide dodge
    grain-extract grain-merge hard-light hue lighten-only multiply
    normal saturation screen subtract value))

(define blend-modes
  (append testable-blend-modes '(dissolve soft-light)))

(define blend-mode-dirs
  '((addition . "addition") (burn . "burn") (color . "color") (darken-only . "darken_only")
    (difference . "difference") (dissolve . "dissolve") (divide . "divide") (dodge . "dodge")
    (grain-extract . "grain_extract") (grain-merge . "grain_merge") (hard-light . "hardlight") (hue . "hue")
    (lighten-only . "lighten_only") (multiply . "multiply") (normal . "normal") (saturation . "saturation")
    (screen . "screen") (soft-light . "softlight") (subtract . "subtract") (value . "value")))

(define alpha-levels
  '("0.2" "0.43" "0.6" "0.78" "1.0"))

(define (compare-image-files ref-file test-file
                             #!key (sample-xs '(4 12 20 28 36 44 52 60 68 76))
                                   (sample-ys '(2 6 10 14 18 22 26 30 34 38))
                                   (compare =))
  (let* ((ref (image-load ref-file))
         (test (image-load test-file))
         (width (image-width ref))
         (height (image-height ref)))
    (unless (and (= (image-width test) width) (= (image-height test) height))
      (error (sprintf "Image dimensions do not match: ~A, ~A" ref-file test-file)))
    (let hloop ((xs sample-xs) (unequal 0))
      (if (null? xs)
        (begin
          (image-destroy ref)
          (image-destroy test)
          unequal)
        (let ((x (car xs)))
          (let vloop ((ys sample-ys) (unequal unequal))
            (if (null? ys)
              (hloop (cdr xs) unequal)
              (let ((y (car ys)))
                (let-values (((ref-r ref-g ref-b ref-a) (image-pixel/rgba ref x y))
                             ((test-r test-g test-b test-a) (image-pixel/rgba test x y)))
                  (if (and (compare test-r ref-r) 
                           (compare test-g ref-g) 
                           (compare test-b ref-b) 
                           (compare test-a ref-a))
                    (vloop (cdr ys) unequal)
                    (vloop (cdr ys) (+ unequal 1))))))))))))

(define (file-path base mode color alpha)
  (make-pathname
    (if (eqv? base 'ref) ref-base-path test-base-path)
    (make-pathname
      mode
      (make-pathname
        color
        (make-pathname alpha test-filename)))))
        
(define (ref-file-path mode color alpha)
  (file-path 'ref mode color alpha))
        
(define (test-file-path mode color alpha)
  (file-path 'test mode color alpha))

(define (create-test-image src mode color alpha)
  (let* ((mode-dir (alist-ref mode blend-mode-dirs))
         (path (test-file-path mode-dir color alpha))
         (cspec (string-append "#" color)) 
         (alpha* (string->number alpha))
         (blend (mk-blend-op cspec blend-mode: mode alpha: alpha*))
         (img (colorize src blend)))
    (create-directory (pathname-directory path) #t)
    (image-save img path)
    (image-destroy img)))

(define (compare-image mode color alpha)
  (let* ((mode-dir (alist-ref mode blend-mode-dirs))
         (rpath (ref-file-path mode-dir color alpha))
         (tpath (test-file-path mode-dir color alpha)))
    (compare-image-files rpath tpath)))

(define (create-test-images) 
  (printf ":::: Generating ~A test images ... this may take a few minutes! ::::::::::::\n"
          (* (length colors) (length blend-modes) (length alpha-levels)))
  (let ((count 0)
        (src (image-load base-image)))
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
    (print ":::: DONE ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    (image-destroy src)))
         
(define (setup)
  (let ((start (current-directory)))
    (change-directory "images")
    (print ":::: Unpacking reference images ::::::::::::::::::::::::::::::::::::::::::::::")
    (unpack "ref.b64.gz" "colors.png" "ref")
    (print ":::: DONE ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    (change-directory start))
  (create-test-images))

; (setup)

(test-group "[1] addition"
  (test-group "[1.01] addition/000000"
    (test
      "1.01.01: addition/000000/0.2"
      0
      (compare-image 'addition "000000" "0.2"))
    (test
      "1.01.02: addition/000000/0.43"
      0
      (compare-image 'addition "000000" "0.43"))
    (test
      "1.01.03: addition/000000/0.6"
      0
      (compare-image 'addition "000000" "0.6"))
    (test
      "1.01.04: addition/000000/0.78"
      0
      (compare-image 'addition "000000" "0.78"))
    (test
      "1.01.05: addition/000000/1.0"
      0
      (compare-image 'addition "000000" "1.0"))
  )
  (test-group "[1.02] addition/0000ff"
    (test
      "1.02.01: addition/0000ff/0.2"
      0
      (compare-image 'addition "0000ff" "0.2"))
    (test
      "1.02.02: addition/0000ff/0.43"
      0
      (compare-image 'addition "0000ff" "0.43"))
    (test
      "1.02.03: addition/0000ff/0.6"
      0
      (compare-image 'addition "0000ff" "0.6"))
    (test
      "1.02.04: addition/0000ff/0.78"
      0
      (compare-image 'addition "0000ff" "0.78"))
    (test
      "1.02.05: addition/0000ff/1.0"
      0
      (compare-image 'addition "0000ff" "1.0"))
  )
  (test-group "[1.03] addition/00ff00"
    (test
      "1.03.01: addition/00ff00/0.2"
      0
      (compare-image 'addition "00ff00" "0.2"))
    (test
      "1.03.02: addition/00ff00/0.43"
      0
      (compare-image 'addition "00ff00" "0.43"))
    (test
      "1.03.03: addition/00ff00/0.6"
      0
      (compare-image 'addition "00ff00" "0.6"))
    (test
      "1.03.04: addition/00ff00/0.78"
      0
      (compare-image 'addition "00ff00" "0.78"))
    (test
      "1.03.05: addition/00ff00/1.0"
      0
      (compare-image 'addition "00ff00" "1.0"))
  )
  (test-group "[1.04] addition/27249c"
    (test
      "1.04.01: addition/27249c/0.2"
      0
      (compare-image 'addition "27249c" "0.2"))
    (test
      "1.04.02: addition/27249c/0.43"
      0
      (compare-image 'addition "27249c" "0.43"))
    (test
      "1.04.03: addition/27249c/0.6"
      0
      (compare-image 'addition "27249c" "0.6"))
    (test
      "1.04.04: addition/27249c/0.78"
      0
      (compare-image 'addition "27249c" "0.78"))
    (test
      "1.04.05: addition/27249c/1.0"
      0
      (compare-image 'addition "27249c" "1.0"))
  )
  (test-group "[1.05] addition/54fa0d"
    (test
      "1.05.01: addition/54fa0d/0.2"
      0
      (compare-image 'addition "54fa0d" "0.2"))
    (test
      "1.05.02: addition/54fa0d/0.43"
      0
      (compare-image 'addition "54fa0d" "0.43"))
    (test
      "1.05.03: addition/54fa0d/0.6"
      0
      (compare-image 'addition "54fa0d" "0.6"))
    (test
      "1.05.04: addition/54fa0d/0.78"
      0
      (compare-image 'addition "54fa0d" "0.78"))
    (test
      "1.05.05: addition/54fa0d/1.0"
      0
      (compare-image 'addition "54fa0d" "1.0"))
  )
  (test-group "[1.06] addition/5c5c5c"
    (test
      "1.06.01: addition/5c5c5c/0.2"
      0
      (compare-image 'addition "5c5c5c" "0.2"))
    (test
      "1.06.02: addition/5c5c5c/0.43"
      0
      (compare-image 'addition "5c5c5c" "0.43"))
    (test
      "1.06.03: addition/5c5c5c/0.6"
      0
      (compare-image 'addition "5c5c5c" "0.6"))
    (test
      "1.06.04: addition/5c5c5c/0.78"
      0
      (compare-image 'addition "5c5c5c" "0.78"))
    (test
      "1.06.05: addition/5c5c5c/1.0"
      0
      (compare-image 'addition "5c5c5c" "1.0"))
  )
  (test-group "[1.07] addition/775acf"
    (test
      "1.07.01: addition/775acf/0.2"
      0
      (compare-image 'addition "775acf" "0.2"))
    (test
      "1.07.02: addition/775acf/0.43"
      0
      (compare-image 'addition "775acf" "0.43"))
    (test
      "1.07.03: addition/775acf/0.6"
      0
      (compare-image 'addition "775acf" "0.6"))
    (test
      "1.07.04: addition/775acf/0.78"
      0
      (compare-image 'addition "775acf" "0.78"))
    (test
      "1.07.05: addition/775acf/1.0"
      0
      (compare-image 'addition "775acf" "1.0"))
  )
  (test-group "[1.08] addition/7f7d0a"
    (test
      "1.08.01: addition/7f7d0a/0.2"
      0
      (compare-image 'addition "7f7d0a" "0.2"))
    (test
      "1.08.02: addition/7f7d0a/0.43"
      0
      (compare-image 'addition "7f7d0a" "0.43"))
    (test
      "1.08.03: addition/7f7d0a/0.6"
      0
      (compare-image 'addition "7f7d0a" "0.6"))
    (test
      "1.08.04: addition/7f7d0a/0.78"
      0
      (compare-image 'addition "7f7d0a" "0.78"))
    (test
      "1.08.05: addition/7f7d0a/1.0"
      0
      (compare-image 'addition "7f7d0a" "1.0"))
  )
  (test-group "[1.09] addition/808080"
    (test
      "1.09.01: addition/808080/0.2"
      0
      (compare-image 'addition "808080" "0.2"))
    (test
      "1.09.02: addition/808080/0.43"
      0
      (compare-image 'addition "808080" "0.43"))
    (test
      "1.09.03: addition/808080/0.6"
      0
      (compare-image 'addition "808080" "0.6"))
    (test
      "1.09.04: addition/808080/0.78"
      0
      (compare-image 'addition "808080" "0.78"))
    (test
      "1.09.05: addition/808080/1.0"
      0
      (compare-image 'addition "808080" "1.0"))
  )
  (test-group "[1.10] addition/899675"
    (test
      "1.10.01: addition/899675/0.2"
      0
      (compare-image 'addition "899675" "0.2"))
    (test
      "1.10.02: addition/899675/0.43"
      0
      (compare-image 'addition "899675" "0.43"))
    (test
      "1.10.03: addition/899675/0.6"
      0
      (compare-image 'addition "899675" "0.6"))
    (test
      "1.10.04: addition/899675/0.78"
      0
      (compare-image 'addition "899675" "0.78"))
    (test
      "1.10.05: addition/899675/1.0"
      0
      (compare-image 'addition "899675" "1.0"))
  )
  (test-group "[1.11] addition/98d5e4"
    (test
      "1.11.01: addition/98d5e4/0.2"
      0
      (compare-image 'addition "98d5e4" "0.2"))
    (test
      "1.11.02: addition/98d5e4/0.43"
      0
      (compare-image 'addition "98d5e4" "0.43"))
    (test
      "1.11.03: addition/98d5e4/0.6"
      0
      (compare-image 'addition "98d5e4" "0.6"))
    (test
      "1.11.04: addition/98d5e4/0.78"
      0
      (compare-image 'addition "98d5e4" "0.78"))
    (test
      "1.11.05: addition/98d5e4/1.0"
      0
      (compare-image 'addition "98d5e4" "1.0"))
  )
  (test-group "[1.12] addition/b5b5b5"
    (test
      "1.12.01: addition/b5b5b5/0.2"
      0
      (compare-image 'addition "b5b5b5" "0.2"))
    (test
      "1.12.02: addition/b5b5b5/0.43"
      0
      (compare-image 'addition "b5b5b5" "0.43"))
    (test
      "1.12.03: addition/b5b5b5/0.6"
      0
      (compare-image 'addition "b5b5b5" "0.6"))
    (test
      "1.12.04: addition/b5b5b5/0.78"
      0
      (compare-image 'addition "b5b5b5" "0.78"))
    (test
      "1.12.05: addition/b5b5b5/1.0"
      0
      (compare-image 'addition "b5b5b5" "1.0"))
  )
  (test-group "[1.13] addition/cd1f3c"
    (test
      "1.13.01: addition/cd1f3c/0.2"
      0
      (compare-image 'addition "cd1f3c" "0.2"))
    (test
      "1.13.02: addition/cd1f3c/0.43"
      0
      (compare-image 'addition "cd1f3c" "0.43"))
    (test
      "1.13.03: addition/cd1f3c/0.6"
      0
      (compare-image 'addition "cd1f3c" "0.6"))
    (test
      "1.13.04: addition/cd1f3c/0.78"
      0
      (compare-image 'addition "cd1f3c" "0.78"))
    (test
      "1.13.05: addition/cd1f3c/1.0"
      0
      (compare-image 'addition "cd1f3c" "1.0"))
  )
  (test-group "[1.14] addition/f62db6"
    (test
      "1.14.01: addition/f62db6/0.2"
      0
      (compare-image 'addition "f62db6" "0.2"))
    (test
      "1.14.02: addition/f62db6/0.43"
      0
      (compare-image 'addition "f62db6" "0.43"))
    (test
      "1.14.03: addition/f62db6/0.6"
      0
      (compare-image 'addition "f62db6" "0.6"))
    (test
      "1.14.04: addition/f62db6/0.78"
      0
      (compare-image 'addition "f62db6" "0.78"))
    (test
      "1.14.05: addition/f62db6/1.0"
      0
      (compare-image 'addition "f62db6" "1.0"))
  )
  (test-group "[1.15] addition/ff0000"
    (test
      "1.15.01: addition/ff0000/0.2"
      0
      (compare-image 'addition "ff0000" "0.2"))
    (test
      "1.15.02: addition/ff0000/0.43"
      0
      (compare-image 'addition "ff0000" "0.43"))
    (test
      "1.15.03: addition/ff0000/0.6"
      0
      (compare-image 'addition "ff0000" "0.6"))
    (test
      "1.15.04: addition/ff0000/0.78"
      0
      (compare-image 'addition "ff0000" "0.78"))
    (test
      "1.15.05: addition/ff0000/1.0"
      0
      (compare-image 'addition "ff0000" "1.0"))
  )
  (test-group "[1.16] addition/ffffff"
    (test
      "1.16.01: addition/ffffff/0.2"
      0
      (compare-image 'addition "ffffff" "0.2"))
    (test
      "1.16.02: addition/ffffff/0.43"
      0
      (compare-image 'addition "ffffff" "0.43"))
    (test
      "1.16.03: addition/ffffff/0.6"
      0
      (compare-image 'addition "ffffff" "0.6"))
    (test
      "1.16.04: addition/ffffff/0.78"
      0
      (compare-image 'addition "ffffff" "0.78"))
    (test
      "1.16.05: addition/ffffff/1.0"
      0
      (compare-image 'addition "ffffff" "1.0"))
  )
)
(test-group "[2] burn"
  (test-group "[2.01] burn/000000"
    (test
      "2.01.01: burn/000000/0.2"
      0
      (compare-image 'burn "000000" "0.2"))
    (test
      "2.01.02: burn/000000/0.43"
      0
      (compare-image 'burn "000000" "0.43"))
    (test
      "2.01.03: burn/000000/0.6"
      0
      (compare-image 'burn "000000" "0.6"))
    (test
      "2.01.04: burn/000000/0.78"
      0
      (compare-image 'burn "000000" "0.78"))
    (test
      "2.01.05: burn/000000/1.0"
      0
      (compare-image 'burn "000000" "1.0"))
  )
  (test-group "[2.02] burn/0000ff"
    (test
      "2.02.01: burn/0000ff/0.2"
      0
      (compare-image 'burn "0000ff" "0.2"))
    (test
      "2.02.02: burn/0000ff/0.43"
      0
      (compare-image 'burn "0000ff" "0.43"))
    (test
      "2.02.03: burn/0000ff/0.6"
      0
      (compare-image 'burn "0000ff" "0.6"))
    (test
      "2.02.04: burn/0000ff/0.78"
      0
      (compare-image 'burn "0000ff" "0.78"))
    (test
      "2.02.05: burn/0000ff/1.0"
      0
      (compare-image 'burn "0000ff" "1.0"))
  )
  (test-group "[2.03] burn/00ff00"
    (test
      "2.03.01: burn/00ff00/0.2"
      0
      (compare-image 'burn "00ff00" "0.2"))
    (test
      "2.03.02: burn/00ff00/0.43"
      0
      (compare-image 'burn "00ff00" "0.43"))
    (test
      "2.03.03: burn/00ff00/0.6"
      0
      (compare-image 'burn "00ff00" "0.6"))
    (test
      "2.03.04: burn/00ff00/0.78"
      0
      (compare-image 'burn "00ff00" "0.78"))
    (test
      "2.03.05: burn/00ff00/1.0"
      0
      (compare-image 'burn "00ff00" "1.0"))
  )
  (test-group "[2.04] burn/27249c"
    (test
      "2.04.01: burn/27249c/0.2"
      0
      (compare-image 'burn "27249c" "0.2"))
    (test
      "2.04.02: burn/27249c/0.43"
      0
      (compare-image 'burn "27249c" "0.43"))
    (test
      "2.04.03: burn/27249c/0.6"
      0
      (compare-image 'burn "27249c" "0.6"))
    (test
      "2.04.04: burn/27249c/0.78"
      0
      (compare-image 'burn "27249c" "0.78"))
    (test
      "2.04.05: burn/27249c/1.0"
      0
      (compare-image 'burn "27249c" "1.0"))
  )
  (test-group "[2.05] burn/54fa0d"
    (test
      "2.05.01: burn/54fa0d/0.2"
      0
      (compare-image 'burn "54fa0d" "0.2"))
    (test
      "2.05.02: burn/54fa0d/0.43"
      0
      (compare-image 'burn "54fa0d" "0.43"))
    (test
      "2.05.03: burn/54fa0d/0.6"
      0
      (compare-image 'burn "54fa0d" "0.6"))
    (test
      "2.05.04: burn/54fa0d/0.78"
      0
      (compare-image 'burn "54fa0d" "0.78"))
    (test
      "2.05.05: burn/54fa0d/1.0"
      0
      (compare-image 'burn "54fa0d" "1.0"))
  )
  (test-group "[2.06] burn/5c5c5c"
    (test
      "2.06.01: burn/5c5c5c/0.2"
      0
      (compare-image 'burn "5c5c5c" "0.2"))
    (test
      "2.06.02: burn/5c5c5c/0.43"
      0
      (compare-image 'burn "5c5c5c" "0.43"))
    (test
      "2.06.03: burn/5c5c5c/0.6"
      0
      (compare-image 'burn "5c5c5c" "0.6"))
    (test
      "2.06.04: burn/5c5c5c/0.78"
      0
      (compare-image 'burn "5c5c5c" "0.78"))
    (test
      "2.06.05: burn/5c5c5c/1.0"
      0
      (compare-image 'burn "5c5c5c" "1.0"))
  )
  (test-group "[2.07] burn/775acf"
    (test
      "2.07.01: burn/775acf/0.2"
      0
      (compare-image 'burn "775acf" "0.2"))
    (test
      "2.07.02: burn/775acf/0.43"
      0
      (compare-image 'burn "775acf" "0.43"))
    (test
      "2.07.03: burn/775acf/0.6"
      0
      (compare-image 'burn "775acf" "0.6"))
    (test
      "2.07.04: burn/775acf/0.78"
      0
      (compare-image 'burn "775acf" "0.78"))
    (test
      "2.07.05: burn/775acf/1.0"
      0
      (compare-image 'burn "775acf" "1.0"))
  )
  (test-group "[2.08] burn/7f7d0a"
    (test
      "2.08.01: burn/7f7d0a/0.2"
      0
      (compare-image 'burn "7f7d0a" "0.2"))
    (test
      "2.08.02: burn/7f7d0a/0.43"
      0
      (compare-image 'burn "7f7d0a" "0.43"))
    (test
      "2.08.03: burn/7f7d0a/0.6"
      0
      (compare-image 'burn "7f7d0a" "0.6"))
    (test
      "2.08.04: burn/7f7d0a/0.78"
      0
      (compare-image 'burn "7f7d0a" "0.78"))
    (test
      "2.08.05: burn/7f7d0a/1.0"
      0
      (compare-image 'burn "7f7d0a" "1.0"))
  )
  (test-group "[2.09] burn/808080"
    (test
      "2.09.01: burn/808080/0.2"
      0
      (compare-image 'burn "808080" "0.2"))
    (test
      "2.09.02: burn/808080/0.43"
      0
      (compare-image 'burn "808080" "0.43"))
    (test
      "2.09.03: burn/808080/0.6"
      0
      (compare-image 'burn "808080" "0.6"))
    (test
      "2.09.04: burn/808080/0.78"
      0
      (compare-image 'burn "808080" "0.78"))
    (test
      "2.09.05: burn/808080/1.0"
      0
      (compare-image 'burn "808080" "1.0"))
  )
  (test-group "[2.10] burn/899675"
    (test
      "2.10.01: burn/899675/0.2"
      0
      (compare-image 'burn "899675" "0.2"))
    (test
      "2.10.02: burn/899675/0.43"
      0
      (compare-image 'burn "899675" "0.43"))
    (test
      "2.10.03: burn/899675/0.6"
      0
      (compare-image 'burn "899675" "0.6"))
    (test
      "2.10.04: burn/899675/0.78"
      0
      (compare-image 'burn "899675" "0.78"))
    (test
      "2.10.05: burn/899675/1.0"
      0
      (compare-image 'burn "899675" "1.0"))
  )
  (test-group "[2.11] burn/98d5e4"
    (test
      "2.11.01: burn/98d5e4/0.2"
      0
      (compare-image 'burn "98d5e4" "0.2"))
    (test
      "2.11.02: burn/98d5e4/0.43"
      0
      (compare-image 'burn "98d5e4" "0.43"))
    (test
      "2.11.03: burn/98d5e4/0.6"
      0
      (compare-image 'burn "98d5e4" "0.6"))
    (test
      "2.11.04: burn/98d5e4/0.78"
      0
      (compare-image 'burn "98d5e4" "0.78"))
    (test
      "2.11.05: burn/98d5e4/1.0"
      0
      (compare-image 'burn "98d5e4" "1.0"))
  )
  (test-group "[2.12] burn/b5b5b5"
    (test
      "2.12.01: burn/b5b5b5/0.2"
      0
      (compare-image 'burn "b5b5b5" "0.2"))
    (test
      "2.12.02: burn/b5b5b5/0.43"
      0
      (compare-image 'burn "b5b5b5" "0.43"))
    (test
      "2.12.03: burn/b5b5b5/0.6"
      0
      (compare-image 'burn "b5b5b5" "0.6"))
    (test
      "2.12.04: burn/b5b5b5/0.78"
      0
      (compare-image 'burn "b5b5b5" "0.78"))
    (test
      "2.12.05: burn/b5b5b5/1.0"
      0
      (compare-image 'burn "b5b5b5" "1.0"))
  )
  (test-group "[2.13] burn/cd1f3c"
    (test
      "2.13.01: burn/cd1f3c/0.2"
      0
      (compare-image 'burn "cd1f3c" "0.2"))
    (test
      "2.13.02: burn/cd1f3c/0.43"
      0
      (compare-image 'burn "cd1f3c" "0.43"))
    (test
      "2.13.03: burn/cd1f3c/0.6"
      0
      (compare-image 'burn "cd1f3c" "0.6"))
    (test
      "2.13.04: burn/cd1f3c/0.78"
      0
      (compare-image 'burn "cd1f3c" "0.78"))
    (test
      "2.13.05: burn/cd1f3c/1.0"
      0
      (compare-image 'burn "cd1f3c" "1.0"))
  )
  (test-group "[2.14] burn/f62db6"
    (test
      "2.14.01: burn/f62db6/0.2"
      0
      (compare-image 'burn "f62db6" "0.2"))
    (test
      "2.14.02: burn/f62db6/0.43"
      0
      (compare-image 'burn "f62db6" "0.43"))
    (test
      "2.14.03: burn/f62db6/0.6"
      0
      (compare-image 'burn "f62db6" "0.6"))
    (test
      "2.14.04: burn/f62db6/0.78"
      0
      (compare-image 'burn "f62db6" "0.78"))
    (test
      "2.14.05: burn/f62db6/1.0"
      0
      (compare-image 'burn "f62db6" "1.0"))
  )
  (test-group "[2.15] burn/ff0000"
    (test
      "2.15.01: burn/ff0000/0.2"
      0
      (compare-image 'burn "ff0000" "0.2"))
    (test
      "2.15.02: burn/ff0000/0.43"
      0
      (compare-image 'burn "ff0000" "0.43"))
    (test
      "2.15.03: burn/ff0000/0.6"
      0
      (compare-image 'burn "ff0000" "0.6"))
    (test
      "2.15.04: burn/ff0000/0.78"
      0
      (compare-image 'burn "ff0000" "0.78"))
    (test
      "2.15.05: burn/ff0000/1.0"
      0
      (compare-image 'burn "ff0000" "1.0"))
  )
  (test-group "[2.16] burn/ffffff"
    (test
      "2.16.01: burn/ffffff/0.2"
      0
      (compare-image 'burn "ffffff" "0.2"))
    (test
      "2.16.02: burn/ffffff/0.43"
      0
      (compare-image 'burn "ffffff" "0.43"))
    (test
      "2.16.03: burn/ffffff/0.6"
      0
      (compare-image 'burn "ffffff" "0.6"))
    (test
      "2.16.04: burn/ffffff/0.78"
      0
      (compare-image 'burn "ffffff" "0.78"))
    (test
      "2.16.05: burn/ffffff/1.0"
      0
      (compare-image 'burn "ffffff" "1.0"))
  )
)
(test-group "[3] color"
  (test-group "[3.01] color/000000"
    (test
      "3.01.01: color/000000/0.2"
      0
      (compare-image 'color "000000" "0.2"))
    (test
      "3.01.02: color/000000/0.43"
      0
      (compare-image 'color "000000" "0.43"))
    (test
      "3.01.03: color/000000/0.6"
      0
      (compare-image 'color "000000" "0.6"))
    (test
      "3.01.04: color/000000/0.78"
      0
      (compare-image 'color "000000" "0.78"))
    (test
      "3.01.05: color/000000/1.0"
      0
      (compare-image 'color "000000" "1.0"))
  )
  (test-group "[3.02] color/0000ff"
    (test
      "3.02.01: color/0000ff/0.2"
      0
      (compare-image 'color "0000ff" "0.2"))
    (test
      "3.02.02: color/0000ff/0.43"
      0
      (compare-image 'color "0000ff" "0.43"))
    (test
      "3.02.03: color/0000ff/0.6"
      0
      (compare-image 'color "0000ff" "0.6"))
    (test
      "3.02.04: color/0000ff/0.78"
      0
      (compare-image 'color "0000ff" "0.78"))
    (test
      "3.02.05: color/0000ff/1.0"
      0
      (compare-image 'color "0000ff" "1.0"))
  )
  (test-group "[3.03] color/00ff00"
    (test
      "3.03.01: color/00ff00/0.2"
      0
      (compare-image 'color "00ff00" "0.2"))
    (test
      "3.03.02: color/00ff00/0.43"
      0
      (compare-image 'color "00ff00" "0.43"))
    (test
      "3.03.03: color/00ff00/0.6"
      0
      (compare-image 'color "00ff00" "0.6"))
    (test
      "3.03.04: color/00ff00/0.78"
      0
      (compare-image 'color "00ff00" "0.78"))
    (test
      "3.03.05: color/00ff00/1.0"
      0
      (compare-image 'color "00ff00" "1.0"))
  )
  (test-group "[3.04] color/27249c"
    (test
      "3.04.01: color/27249c/0.2"
      0
      (compare-image 'color "27249c" "0.2"))
    (test
      "3.04.02: color/27249c/0.43"
      0
      (compare-image 'color "27249c" "0.43"))
    (test
      "3.04.03: color/27249c/0.6"
      0
      (compare-image 'color "27249c" "0.6"))
    (test
      "3.04.04: color/27249c/0.78"
      0
      (compare-image 'color "27249c" "0.78"))
    (test
      "3.04.05: color/27249c/1.0"
      0
      (compare-image 'color "27249c" "1.0"))
  )
  (test-group "[3.05] color/54fa0d"
    (test
      "3.05.01: color/54fa0d/0.2"
      0
      (compare-image 'color "54fa0d" "0.2"))
    (test
      "3.05.02: color/54fa0d/0.43"
      0
      (compare-image 'color "54fa0d" "0.43"))
    (test
      "3.05.03: color/54fa0d/0.6"
      0
      (compare-image 'color "54fa0d" "0.6"))
    (test
      "3.05.04: color/54fa0d/0.78"
      0
      (compare-image 'color "54fa0d" "0.78"))
    (test
      "3.05.05: color/54fa0d/1.0"
      0
      (compare-image 'color "54fa0d" "1.0"))
  )
  (test-group "[3.06] color/5c5c5c"
    (test
      "3.06.01: color/5c5c5c/0.2"
      0
      (compare-image 'color "5c5c5c" "0.2"))
    (test
      "3.06.02: color/5c5c5c/0.43"
      0
      (compare-image 'color "5c5c5c" "0.43"))
    (test
      "3.06.03: color/5c5c5c/0.6"
      0
      (compare-image 'color "5c5c5c" "0.6"))
    (test
      "3.06.04: color/5c5c5c/0.78"
      0
      (compare-image 'color "5c5c5c" "0.78"))
    (test
      "3.06.05: color/5c5c5c/1.0"
      0
      (compare-image 'color "5c5c5c" "1.0"))
  )
  (test-group "[3.07] color/775acf"
    (test
      "3.07.01: color/775acf/0.2"
      0
      (compare-image 'color "775acf" "0.2"))
    (test
      "3.07.02: color/775acf/0.43"
      0
      (compare-image 'color "775acf" "0.43"))
    (test
      "3.07.03: color/775acf/0.6"
      0
      (compare-image 'color "775acf" "0.6"))
    (test
      "3.07.04: color/775acf/0.78"
      0
      (compare-image 'color "775acf" "0.78"))
    (test
      "3.07.05: color/775acf/1.0"
      0
      (compare-image 'color "775acf" "1.0"))
  )
  (test-group "[3.08] color/7f7d0a"
    (test
      "3.08.01: color/7f7d0a/0.2"
      0
      (compare-image 'color "7f7d0a" "0.2"))
    (test
      "3.08.02: color/7f7d0a/0.43"
      0
      (compare-image 'color "7f7d0a" "0.43"))
    (test
      "3.08.03: color/7f7d0a/0.6"
      0
      (compare-image 'color "7f7d0a" "0.6"))
    (test
      "3.08.04: color/7f7d0a/0.78"
      0
      (compare-image 'color "7f7d0a" "0.78"))
    (test
      "3.08.05: color/7f7d0a/1.0"
      0
      (compare-image 'color "7f7d0a" "1.0"))
  )
  (test-group "[3.09] color/808080"
    (test
      "3.09.01: color/808080/0.2"
      0
      (compare-image 'color "808080" "0.2"))
    (test
      "3.09.02: color/808080/0.43"
      0
      (compare-image 'color "808080" "0.43"))
    (test
      "3.09.03: color/808080/0.6"
      0
      (compare-image 'color "808080" "0.6"))
    (test
      "3.09.04: color/808080/0.78"
      0
      (compare-image 'color "808080" "0.78"))
    (test
      "3.09.05: color/808080/1.0"
      0
      (compare-image 'color "808080" "1.0"))
  )
  (test-group "[3.10] color/899675"
    (test
      "3.10.01: color/899675/0.2"
      0
      (compare-image 'color "899675" "0.2"))
    (test
      "3.10.02: color/899675/0.43"
      0
      (compare-image 'color "899675" "0.43"))
    (test
      "3.10.03: color/899675/0.6"
      0
      (compare-image 'color "899675" "0.6"))
    (test
      "3.10.04: color/899675/0.78"
      0
      (compare-image 'color "899675" "0.78"))
    (test
      "3.10.05: color/899675/1.0"
      0
      (compare-image 'color "899675" "1.0"))
  )
  (test-group "[3.11] color/98d5e4"
    (test
      "3.11.01: color/98d5e4/0.2"
      0
      (compare-image 'color "98d5e4" "0.2"))
    (test
      "3.11.02: color/98d5e4/0.43"
      0
      (compare-image 'color "98d5e4" "0.43"))
    (test
      "3.11.03: color/98d5e4/0.6"
      0
      (compare-image 'color "98d5e4" "0.6"))
    (test
      "3.11.04: color/98d5e4/0.78"
      0
      (compare-image 'color "98d5e4" "0.78"))
    (test
      "3.11.05: color/98d5e4/1.0"
      0
      (compare-image 'color "98d5e4" "1.0"))
  )
  (test-group "[3.12] color/b5b5b5"
    (test
      "3.12.01: color/b5b5b5/0.2"
      0
      (compare-image 'color "b5b5b5" "0.2"))
    (test
      "3.12.02: color/b5b5b5/0.43"
      0
      (compare-image 'color "b5b5b5" "0.43"))
    (test
      "3.12.03: color/b5b5b5/0.6"
      0
      (compare-image 'color "b5b5b5" "0.6"))
    (test
      "3.12.04: color/b5b5b5/0.78"
      0
      (compare-image 'color "b5b5b5" "0.78"))
    (test
      "3.12.05: color/b5b5b5/1.0"
      0
      (compare-image 'color "b5b5b5" "1.0"))
  )
  (test-group "[3.13] color/cd1f3c"
    (test
      "3.13.01: color/cd1f3c/0.2"
      0
      (compare-image 'color "cd1f3c" "0.2"))
    (test
      "3.13.02: color/cd1f3c/0.43"
      0
      (compare-image 'color "cd1f3c" "0.43"))
    (test
      "3.13.03: color/cd1f3c/0.6"
      0
      (compare-image 'color "cd1f3c" "0.6"))
    (test
      "3.13.04: color/cd1f3c/0.78"
      0
      (compare-image 'color "cd1f3c" "0.78"))
    (test
      "3.13.05: color/cd1f3c/1.0"
      0
      (compare-image 'color "cd1f3c" "1.0"))
  )
  (test-group "[3.14] color/f62db6"
    (test
      "3.14.01: color/f62db6/0.2"
      0
      (compare-image 'color "f62db6" "0.2"))
    (test
      "3.14.02: color/f62db6/0.43"
      0
      (compare-image 'color "f62db6" "0.43"))
    (test
      "3.14.03: color/f62db6/0.6"
      0
      (compare-image 'color "f62db6" "0.6"))
    (test
      "3.14.04: color/f62db6/0.78"
      0
      (compare-image 'color "f62db6" "0.78"))
    (test
      "3.14.05: color/f62db6/1.0"
      0
      (compare-image 'color "f62db6" "1.0"))
  )
  (test-group "[3.15] color/ff0000"
    (test
      "3.15.01: color/ff0000/0.2"
      0
      (compare-image 'color "ff0000" "0.2"))
    (test
      "3.15.02: color/ff0000/0.43"
      0
      (compare-image 'color "ff0000" "0.43"))
    (test
      "3.15.03: color/ff0000/0.6"
      0
      (compare-image 'color "ff0000" "0.6"))
    (test
      "3.15.04: color/ff0000/0.78"
      0
      (compare-image 'color "ff0000" "0.78"))
    (test
      "3.15.05: color/ff0000/1.0"
      0
      (compare-image 'color "ff0000" "1.0"))
  )
  (test-group "[3.16] color/ffffff"
    (test
      "3.16.01: color/ffffff/0.2"
      0
      (compare-image 'color "ffffff" "0.2"))
    (test
      "3.16.02: color/ffffff/0.43"
      0
      (compare-image 'color "ffffff" "0.43"))
    (test
      "3.16.03: color/ffffff/0.6"
      0
      (compare-image 'color "ffffff" "0.6"))
    (test
      "3.16.04: color/ffffff/0.78"
      0
      (compare-image 'color "ffffff" "0.78"))
    (test
      "3.16.05: color/ffffff/1.0"
      0
      (compare-image 'color "ffffff" "1.0"))
  )
)
(test-group "[4] darken-only"
  (test-group "[4.01] darken-only/000000"
    (test
      "4.01.01: darken-only/000000/0.2"
      0
      (compare-image 'darken-only "000000" "0.2"))
    (test
      "4.01.02: darken-only/000000/0.43"
      0
      (compare-image 'darken-only "000000" "0.43"))
    (test
      "4.01.03: darken-only/000000/0.6"
      0
      (compare-image 'darken-only "000000" "0.6"))
    (test
      "4.01.04: darken-only/000000/0.78"
      0
      (compare-image 'darken-only "000000" "0.78"))
    (test
      "4.01.05: darken-only/000000/1.0"
      0
      (compare-image 'darken-only "000000" "1.0"))
  )
  (test-group "[4.02] darken-only/0000ff"
    (test
      "4.02.01: darken-only/0000ff/0.2"
      0
      (compare-image 'darken-only "0000ff" "0.2"))
    (test
      "4.02.02: darken-only/0000ff/0.43"
      0
      (compare-image 'darken-only "0000ff" "0.43"))
    (test
      "4.02.03: darken-only/0000ff/0.6"
      0
      (compare-image 'darken-only "0000ff" "0.6"))
    (test
      "4.02.04: darken-only/0000ff/0.78"
      0
      (compare-image 'darken-only "0000ff" "0.78"))
    (test
      "4.02.05: darken-only/0000ff/1.0"
      0
      (compare-image 'darken-only "0000ff" "1.0"))
  )
  (test-group "[4.03] darken-only/00ff00"
    (test
      "4.03.01: darken-only/00ff00/0.2"
      0
      (compare-image 'darken-only "00ff00" "0.2"))
    (test
      "4.03.02: darken-only/00ff00/0.43"
      0
      (compare-image 'darken-only "00ff00" "0.43"))
    (test
      "4.03.03: darken-only/00ff00/0.6"
      0
      (compare-image 'darken-only "00ff00" "0.6"))
    (test
      "4.03.04: darken-only/00ff00/0.78"
      0
      (compare-image 'darken-only "00ff00" "0.78"))
    (test
      "4.03.05: darken-only/00ff00/1.0"
      0
      (compare-image 'darken-only "00ff00" "1.0"))
  )
  (test-group "[4.04] darken-only/27249c"
    (test
      "4.04.01: darken-only/27249c/0.2"
      0
      (compare-image 'darken-only "27249c" "0.2"))
    (test
      "4.04.02: darken-only/27249c/0.43"
      0
      (compare-image 'darken-only "27249c" "0.43"))
    (test
      "4.04.03: darken-only/27249c/0.6"
      0
      (compare-image 'darken-only "27249c" "0.6"))
    (test
      "4.04.04: darken-only/27249c/0.78"
      0
      (compare-image 'darken-only "27249c" "0.78"))
    (test
      "4.04.05: darken-only/27249c/1.0"
      0
      (compare-image 'darken-only "27249c" "1.0"))
  )
  (test-group "[4.05] darken-only/54fa0d"
    (test
      "4.05.01: darken-only/54fa0d/0.2"
      0
      (compare-image 'darken-only "54fa0d" "0.2"))
    (test
      "4.05.02: darken-only/54fa0d/0.43"
      0
      (compare-image 'darken-only "54fa0d" "0.43"))
    (test
      "4.05.03: darken-only/54fa0d/0.6"
      0
      (compare-image 'darken-only "54fa0d" "0.6"))
    (test
      "4.05.04: darken-only/54fa0d/0.78"
      0
      (compare-image 'darken-only "54fa0d" "0.78"))
    (test
      "4.05.05: darken-only/54fa0d/1.0"
      0
      (compare-image 'darken-only "54fa0d" "1.0"))
  )
  (test-group "[4.06] darken-only/5c5c5c"
    (test
      "4.06.01: darken-only/5c5c5c/0.2"
      0
      (compare-image 'darken-only "5c5c5c" "0.2"))
    (test
      "4.06.02: darken-only/5c5c5c/0.43"
      0
      (compare-image 'darken-only "5c5c5c" "0.43"))
    (test
      "4.06.03: darken-only/5c5c5c/0.6"
      0
      (compare-image 'darken-only "5c5c5c" "0.6"))
    (test
      "4.06.04: darken-only/5c5c5c/0.78"
      0
      (compare-image 'darken-only "5c5c5c" "0.78"))
    (test
      "4.06.05: darken-only/5c5c5c/1.0"
      0
      (compare-image 'darken-only "5c5c5c" "1.0"))
  )
  (test-group "[4.07] darken-only/775acf"
    (test
      "4.07.01: darken-only/775acf/0.2"
      0
      (compare-image 'darken-only "775acf" "0.2"))
    (test
      "4.07.02: darken-only/775acf/0.43"
      0
      (compare-image 'darken-only "775acf" "0.43"))
    (test
      "4.07.03: darken-only/775acf/0.6"
      0
      (compare-image 'darken-only "775acf" "0.6"))
    (test
      "4.07.04: darken-only/775acf/0.78"
      0
      (compare-image 'darken-only "775acf" "0.78"))
    (test
      "4.07.05: darken-only/775acf/1.0"
      0
      (compare-image 'darken-only "775acf" "1.0"))
  )
  (test-group "[4.08] darken-only/7f7d0a"
    (test
      "4.08.01: darken-only/7f7d0a/0.2"
      0
      (compare-image 'darken-only "7f7d0a" "0.2"))
    (test
      "4.08.02: darken-only/7f7d0a/0.43"
      0
      (compare-image 'darken-only "7f7d0a" "0.43"))
    (test
      "4.08.03: darken-only/7f7d0a/0.6"
      0
      (compare-image 'darken-only "7f7d0a" "0.6"))
    (test
      "4.08.04: darken-only/7f7d0a/0.78"
      0
      (compare-image 'darken-only "7f7d0a" "0.78"))
    (test
      "4.08.05: darken-only/7f7d0a/1.0"
      0
      (compare-image 'darken-only "7f7d0a" "1.0"))
  )
  (test-group "[4.09] darken-only/808080"
    (test
      "4.09.01: darken-only/808080/0.2"
      0
      (compare-image 'darken-only "808080" "0.2"))
    (test
      "4.09.02: darken-only/808080/0.43"
      0
      (compare-image 'darken-only "808080" "0.43"))
    (test
      "4.09.03: darken-only/808080/0.6"
      0
      (compare-image 'darken-only "808080" "0.6"))
    (test
      "4.09.04: darken-only/808080/0.78"
      0
      (compare-image 'darken-only "808080" "0.78"))
    (test
      "4.09.05: darken-only/808080/1.0"
      0
      (compare-image 'darken-only "808080" "1.0"))
  )
  (test-group "[4.10] darken-only/899675"
    (test
      "4.10.01: darken-only/899675/0.2"
      0
      (compare-image 'darken-only "899675" "0.2"))
    (test
      "4.10.02: darken-only/899675/0.43"
      0
      (compare-image 'darken-only "899675" "0.43"))
    (test
      "4.10.03: darken-only/899675/0.6"
      0
      (compare-image 'darken-only "899675" "0.6"))
    (test
      "4.10.04: darken-only/899675/0.78"
      0
      (compare-image 'darken-only "899675" "0.78"))
    (test
      "4.10.05: darken-only/899675/1.0"
      0
      (compare-image 'darken-only "899675" "1.0"))
  )
  (test-group "[4.11] darken-only/98d5e4"
    (test
      "4.11.01: darken-only/98d5e4/0.2"
      0
      (compare-image 'darken-only "98d5e4" "0.2"))
    (test
      "4.11.02: darken-only/98d5e4/0.43"
      0
      (compare-image 'darken-only "98d5e4" "0.43"))
    (test
      "4.11.03: darken-only/98d5e4/0.6"
      0
      (compare-image 'darken-only "98d5e4" "0.6"))
    (test
      "4.11.04: darken-only/98d5e4/0.78"
      0
      (compare-image 'darken-only "98d5e4" "0.78"))
    (test
      "4.11.05: darken-only/98d5e4/1.0"
      0
      (compare-image 'darken-only "98d5e4" "1.0"))
  )
  (test-group "[4.12] darken-only/b5b5b5"
    (test
      "4.12.01: darken-only/b5b5b5/0.2"
      0
      (compare-image 'darken-only "b5b5b5" "0.2"))
    (test
      "4.12.02: darken-only/b5b5b5/0.43"
      0
      (compare-image 'darken-only "b5b5b5" "0.43"))
    (test
      "4.12.03: darken-only/b5b5b5/0.6"
      0
      (compare-image 'darken-only "b5b5b5" "0.6"))
    (test
      "4.12.04: darken-only/b5b5b5/0.78"
      0
      (compare-image 'darken-only "b5b5b5" "0.78"))
    (test
      "4.12.05: darken-only/b5b5b5/1.0"
      0
      (compare-image 'darken-only "b5b5b5" "1.0"))
  )
  (test-group "[4.13] darken-only/cd1f3c"
    (test
      "4.13.01: darken-only/cd1f3c/0.2"
      0
      (compare-image 'darken-only "cd1f3c" "0.2"))
    (test
      "4.13.02: darken-only/cd1f3c/0.43"
      0
      (compare-image 'darken-only "cd1f3c" "0.43"))
    (test
      "4.13.03: darken-only/cd1f3c/0.6"
      0
      (compare-image 'darken-only "cd1f3c" "0.6"))
    (test
      "4.13.04: darken-only/cd1f3c/0.78"
      0
      (compare-image 'darken-only "cd1f3c" "0.78"))
    (test
      "4.13.05: darken-only/cd1f3c/1.0"
      0
      (compare-image 'darken-only "cd1f3c" "1.0"))
  )
  (test-group "[4.14] darken-only/f62db6"
    (test
      "4.14.01: darken-only/f62db6/0.2"
      0
      (compare-image 'darken-only "f62db6" "0.2"))
    (test
      "4.14.02: darken-only/f62db6/0.43"
      0
      (compare-image 'darken-only "f62db6" "0.43"))
    (test
      "4.14.03: darken-only/f62db6/0.6"
      0
      (compare-image 'darken-only "f62db6" "0.6"))
    (test
      "4.14.04: darken-only/f62db6/0.78"
      0
      (compare-image 'darken-only "f62db6" "0.78"))
    (test
      "4.14.05: darken-only/f62db6/1.0"
      0
      (compare-image 'darken-only "f62db6" "1.0"))
  )
  (test-group "[4.15] darken-only/ff0000"
    (test
      "4.15.01: darken-only/ff0000/0.2"
      0
      (compare-image 'darken-only "ff0000" "0.2"))
    (test
      "4.15.02: darken-only/ff0000/0.43"
      0
      (compare-image 'darken-only "ff0000" "0.43"))
    (test
      "4.15.03: darken-only/ff0000/0.6"
      0
      (compare-image 'darken-only "ff0000" "0.6"))
    (test
      "4.15.04: darken-only/ff0000/0.78"
      0
      (compare-image 'darken-only "ff0000" "0.78"))
    (test
      "4.15.05: darken-only/ff0000/1.0"
      0
      (compare-image 'darken-only "ff0000" "1.0"))
  )
  (test-group "[4.16] darken-only/ffffff"
    (test
      "4.16.01: darken-only/ffffff/0.2"
      0
      (compare-image 'darken-only "ffffff" "0.2"))
    (test
      "4.16.02: darken-only/ffffff/0.43"
      0
      (compare-image 'darken-only "ffffff" "0.43"))
    (test
      "4.16.03: darken-only/ffffff/0.6"
      0
      (compare-image 'darken-only "ffffff" "0.6"))
    (test
      "4.16.04: darken-only/ffffff/0.78"
      0
      (compare-image 'darken-only "ffffff" "0.78"))
    (test
      "4.16.05: darken-only/ffffff/1.0"
      0
      (compare-image 'darken-only "ffffff" "1.0"))
  )
)
(test-group "[5] difference"
  (test-group "[5.01] difference/000000"
    (test
      "5.01.01: difference/000000/0.2"
      0
      (compare-image 'difference "000000" "0.2"))
    (test
      "5.01.02: difference/000000/0.43"
      0
      (compare-image 'difference "000000" "0.43"))
    (test
      "5.01.03: difference/000000/0.6"
      0
      (compare-image 'difference "000000" "0.6"))
    (test
      "5.01.04: difference/000000/0.78"
      0
      (compare-image 'difference "000000" "0.78"))
    (test
      "5.01.05: difference/000000/1.0"
      0
      (compare-image 'difference "000000" "1.0"))
  )
  (test-group "[5.02] difference/0000ff"
    (test
      "5.02.01: difference/0000ff/0.2"
      0
      (compare-image 'difference "0000ff" "0.2"))
    (test
      "5.02.02: difference/0000ff/0.43"
      0
      (compare-image 'difference "0000ff" "0.43"))
    (test
      "5.02.03: difference/0000ff/0.6"
      0
      (compare-image 'difference "0000ff" "0.6"))
    (test
      "5.02.04: difference/0000ff/0.78"
      0
      (compare-image 'difference "0000ff" "0.78"))
    (test
      "5.02.05: difference/0000ff/1.0"
      0
      (compare-image 'difference "0000ff" "1.0"))
  )
  (test-group "[5.03] difference/00ff00"
    (test
      "5.03.01: difference/00ff00/0.2"
      0
      (compare-image 'difference "00ff00" "0.2"))
    (test
      "5.03.02: difference/00ff00/0.43"
      0
      (compare-image 'difference "00ff00" "0.43"))
    (test
      "5.03.03: difference/00ff00/0.6"
      0
      (compare-image 'difference "00ff00" "0.6"))
    (test
      "5.03.04: difference/00ff00/0.78"
      0
      (compare-image 'difference "00ff00" "0.78"))
    (test
      "5.03.05: difference/00ff00/1.0"
      0
      (compare-image 'difference "00ff00" "1.0"))
  )
  (test-group "[5.04] difference/27249c"
    (test
      "5.04.01: difference/27249c/0.2"
      0
      (compare-image 'difference "27249c" "0.2"))
    (test
      "5.04.02: difference/27249c/0.43"
      0
      (compare-image 'difference "27249c" "0.43"))
    (test
      "5.04.03: difference/27249c/0.6"
      0
      (compare-image 'difference "27249c" "0.6"))
    (test
      "5.04.04: difference/27249c/0.78"
      0
      (compare-image 'difference "27249c" "0.78"))
    (test
      "5.04.05: difference/27249c/1.0"
      0
      (compare-image 'difference "27249c" "1.0"))
  )
  (test-group "[5.05] difference/54fa0d"
    (test
      "5.05.01: difference/54fa0d/0.2"
      0
      (compare-image 'difference "54fa0d" "0.2"))
    (test
      "5.05.02: difference/54fa0d/0.43"
      0
      (compare-image 'difference "54fa0d" "0.43"))
    (test
      "5.05.03: difference/54fa0d/0.6"
      0
      (compare-image 'difference "54fa0d" "0.6"))
    (test
      "5.05.04: difference/54fa0d/0.78"
      0
      (compare-image 'difference "54fa0d" "0.78"))
    (test
      "5.05.05: difference/54fa0d/1.0"
      0
      (compare-image 'difference "54fa0d" "1.0"))
  )
  (test-group "[5.06] difference/5c5c5c"
    (test
      "5.06.01: difference/5c5c5c/0.2"
      0
      (compare-image 'difference "5c5c5c" "0.2"))
    (test
      "5.06.02: difference/5c5c5c/0.43"
      0
      (compare-image 'difference "5c5c5c" "0.43"))
    (test
      "5.06.03: difference/5c5c5c/0.6"
      0
      (compare-image 'difference "5c5c5c" "0.6"))
    (test
      "5.06.04: difference/5c5c5c/0.78"
      0
      (compare-image 'difference "5c5c5c" "0.78"))
    (test
      "5.06.05: difference/5c5c5c/1.0"
      0
      (compare-image 'difference "5c5c5c" "1.0"))
  )
  (test-group "[5.07] difference/775acf"
    (test
      "5.07.01: difference/775acf/0.2"
      0
      (compare-image 'difference "775acf" "0.2"))
    (test
      "5.07.02: difference/775acf/0.43"
      0
      (compare-image 'difference "775acf" "0.43"))
    (test
      "5.07.03: difference/775acf/0.6"
      0
      (compare-image 'difference "775acf" "0.6"))
    (test
      "5.07.04: difference/775acf/0.78"
      0
      (compare-image 'difference "775acf" "0.78"))
    (test
      "5.07.05: difference/775acf/1.0"
      0
      (compare-image 'difference "775acf" "1.0"))
  )
  (test-group "[5.08] difference/7f7d0a"
    (test
      "5.08.01: difference/7f7d0a/0.2"
      0
      (compare-image 'difference "7f7d0a" "0.2"))
    (test
      "5.08.02: difference/7f7d0a/0.43"
      0
      (compare-image 'difference "7f7d0a" "0.43"))
    (test
      "5.08.03: difference/7f7d0a/0.6"
      0
      (compare-image 'difference "7f7d0a" "0.6"))
    (test
      "5.08.04: difference/7f7d0a/0.78"
      0
      (compare-image 'difference "7f7d0a" "0.78"))
    (test
      "5.08.05: difference/7f7d0a/1.0"
      0
      (compare-image 'difference "7f7d0a" "1.0"))
  )
  (test-group "[5.09] difference/808080"
    (test
      "5.09.01: difference/808080/0.2"
      0
      (compare-image 'difference "808080" "0.2"))
    (test
      "5.09.02: difference/808080/0.43"
      0
      (compare-image 'difference "808080" "0.43"))
    (test
      "5.09.03: difference/808080/0.6"
      0
      (compare-image 'difference "808080" "0.6"))
    (test
      "5.09.04: difference/808080/0.78"
      0
      (compare-image 'difference "808080" "0.78"))
    (test
      "5.09.05: difference/808080/1.0"
      0
      (compare-image 'difference "808080" "1.0"))
  )
  (test-group "[5.10] difference/899675"
    (test
      "5.10.01: difference/899675/0.2"
      0
      (compare-image 'difference "899675" "0.2"))
    (test
      "5.10.02: difference/899675/0.43"
      0
      (compare-image 'difference "899675" "0.43"))
    (test
      "5.10.03: difference/899675/0.6"
      0
      (compare-image 'difference "899675" "0.6"))
    (test
      "5.10.04: difference/899675/0.78"
      0
      (compare-image 'difference "899675" "0.78"))
    (test
      "5.10.05: difference/899675/1.0"
      0
      (compare-image 'difference "899675" "1.0"))
  )
  (test-group "[5.11] difference/98d5e4"
    (test
      "5.11.01: difference/98d5e4/0.2"
      0
      (compare-image 'difference "98d5e4" "0.2"))
    (test
      "5.11.02: difference/98d5e4/0.43"
      0
      (compare-image 'difference "98d5e4" "0.43"))
    (test
      "5.11.03: difference/98d5e4/0.6"
      0
      (compare-image 'difference "98d5e4" "0.6"))
    (test
      "5.11.04: difference/98d5e4/0.78"
      0
      (compare-image 'difference "98d5e4" "0.78"))
    (test
      "5.11.05: difference/98d5e4/1.0"
      0
      (compare-image 'difference "98d5e4" "1.0"))
  )
  (test-group "[5.12] difference/b5b5b5"
    (test
      "5.12.01: difference/b5b5b5/0.2"
      0
      (compare-image 'difference "b5b5b5" "0.2"))
    (test
      "5.12.02: difference/b5b5b5/0.43"
      0
      (compare-image 'difference "b5b5b5" "0.43"))
    (test
      "5.12.03: difference/b5b5b5/0.6"
      0
      (compare-image 'difference "b5b5b5" "0.6"))
    (test
      "5.12.04: difference/b5b5b5/0.78"
      0
      (compare-image 'difference "b5b5b5" "0.78"))
    (test
      "5.12.05: difference/b5b5b5/1.0"
      0
      (compare-image 'difference "b5b5b5" "1.0"))
  )
  (test-group "[5.13] difference/cd1f3c"
    (test
      "5.13.01: difference/cd1f3c/0.2"
      0
      (compare-image 'difference "cd1f3c" "0.2"))
    (test
      "5.13.02: difference/cd1f3c/0.43"
      0
      (compare-image 'difference "cd1f3c" "0.43"))
    (test
      "5.13.03: difference/cd1f3c/0.6"
      0
      (compare-image 'difference "cd1f3c" "0.6"))
    (test
      "5.13.04: difference/cd1f3c/0.78"
      0
      (compare-image 'difference "cd1f3c" "0.78"))
    (test
      "5.13.05: difference/cd1f3c/1.0"
      0
      (compare-image 'difference "cd1f3c" "1.0"))
  )
  (test-group "[5.14] difference/f62db6"
    (test
      "5.14.01: difference/f62db6/0.2"
      0
      (compare-image 'difference "f62db6" "0.2"))
    (test
      "5.14.02: difference/f62db6/0.43"
      0
      (compare-image 'difference "f62db6" "0.43"))
    (test
      "5.14.03: difference/f62db6/0.6"
      0
      (compare-image 'difference "f62db6" "0.6"))
    (test
      "5.14.04: difference/f62db6/0.78"
      0
      (compare-image 'difference "f62db6" "0.78"))
    (test
      "5.14.05: difference/f62db6/1.0"
      0
      (compare-image 'difference "f62db6" "1.0"))
  )
  (test-group "[5.15] difference/ff0000"
    (test
      "5.15.01: difference/ff0000/0.2"
      0
      (compare-image 'difference "ff0000" "0.2"))
    (test
      "5.15.02: difference/ff0000/0.43"
      0
      (compare-image 'difference "ff0000" "0.43"))
    (test
      "5.15.03: difference/ff0000/0.6"
      0
      (compare-image 'difference "ff0000" "0.6"))
    (test
      "5.15.04: difference/ff0000/0.78"
      0
      (compare-image 'difference "ff0000" "0.78"))
    (test
      "5.15.05: difference/ff0000/1.0"
      0
      (compare-image 'difference "ff0000" "1.0"))
  )
  (test-group "[5.16] difference/ffffff"
    (test
      "5.16.01: difference/ffffff/0.2"
      0
      (compare-image 'difference "ffffff" "0.2"))
    (test
      "5.16.02: difference/ffffff/0.43"
      0
      (compare-image 'difference "ffffff" "0.43"))
    (test
      "5.16.03: difference/ffffff/0.6"
      0
      (compare-image 'difference "ffffff" "0.6"))
    (test
      "5.16.04: difference/ffffff/0.78"
      0
      (compare-image 'difference "ffffff" "0.78"))
    (test
      "5.16.05: difference/ffffff/1.0"
      0
      (compare-image 'difference "ffffff" "1.0"))
  )
)
(test-group "[6] divide"
  (test-group "[6.01] divide/000000"
    (test
      "6.01.01: divide/000000/0.2"
      0
      (compare-image 'divide "000000" "0.2"))
    (test
      "6.01.02: divide/000000/0.43"
      0
      (compare-image 'divide "000000" "0.43"))
    (test
      "6.01.03: divide/000000/0.6"
      0
      (compare-image 'divide "000000" "0.6"))
    (test
      "6.01.04: divide/000000/0.78"
      0
      (compare-image 'divide "000000" "0.78"))
    (test
      "6.01.05: divide/000000/1.0"
      0
      (compare-image 'divide "000000" "1.0"))
  )
  (test-group "[6.02] divide/0000ff"
    (test
      "6.02.01: divide/0000ff/0.2"
      0
      (compare-image 'divide "0000ff" "0.2"))
    (test
      "6.02.02: divide/0000ff/0.43"
      0
      (compare-image 'divide "0000ff" "0.43"))
    (test
      "6.02.03: divide/0000ff/0.6"
      0
      (compare-image 'divide "0000ff" "0.6"))
    (test
      "6.02.04: divide/0000ff/0.78"
      0
      (compare-image 'divide "0000ff" "0.78"))
    (test
      "6.02.05: divide/0000ff/1.0"
      0
      (compare-image 'divide "0000ff" "1.0"))
  )
  (test-group "[6.03] divide/00ff00"
    (test
      "6.03.01: divide/00ff00/0.2"
      0
      (compare-image 'divide "00ff00" "0.2"))
    (test
      "6.03.02: divide/00ff00/0.43"
      0
      (compare-image 'divide "00ff00" "0.43"))
    (test
      "6.03.03: divide/00ff00/0.6"
      0
      (compare-image 'divide "00ff00" "0.6"))
    (test
      "6.03.04: divide/00ff00/0.78"
      0
      (compare-image 'divide "00ff00" "0.78"))
    (test
      "6.03.05: divide/00ff00/1.0"
      0
      (compare-image 'divide "00ff00" "1.0"))
  )
  (test-group "[6.04] divide/27249c"
    (test
      "6.04.01: divide/27249c/0.2"
      0
      (compare-image 'divide "27249c" "0.2"))
    (test
      "6.04.02: divide/27249c/0.43"
      0
      (compare-image 'divide "27249c" "0.43"))
    (test
      "6.04.03: divide/27249c/0.6"
      0
      (compare-image 'divide "27249c" "0.6"))
    (test
      "6.04.04: divide/27249c/0.78"
      0
      (compare-image 'divide "27249c" "0.78"))
    (test
      "6.04.05: divide/27249c/1.0"
      0
      (compare-image 'divide "27249c" "1.0"))
  )
  (test-group "[6.05] divide/54fa0d"
    (test
      "6.05.01: divide/54fa0d/0.2"
      0
      (compare-image 'divide "54fa0d" "0.2"))
    (test
      "6.05.02: divide/54fa0d/0.43"
      0
      (compare-image 'divide "54fa0d" "0.43"))
    (test
      "6.05.03: divide/54fa0d/0.6"
      0
      (compare-image 'divide "54fa0d" "0.6"))
    (test
      "6.05.04: divide/54fa0d/0.78"
      0
      (compare-image 'divide "54fa0d" "0.78"))
    (test
      "6.05.05: divide/54fa0d/1.0"
      0
      (compare-image 'divide "54fa0d" "1.0"))
  )
  (test-group "[6.06] divide/5c5c5c"
    (test
      "6.06.01: divide/5c5c5c/0.2"
      0
      (compare-image 'divide "5c5c5c" "0.2"))
    (test
      "6.06.02: divide/5c5c5c/0.43"
      0
      (compare-image 'divide "5c5c5c" "0.43"))
    (test
      "6.06.03: divide/5c5c5c/0.6"
      0
      (compare-image 'divide "5c5c5c" "0.6"))
    (test
      "6.06.04: divide/5c5c5c/0.78"
      0
      (compare-image 'divide "5c5c5c" "0.78"))
    (test
      "6.06.05: divide/5c5c5c/1.0"
      0
      (compare-image 'divide "5c5c5c" "1.0"))
  )
  (test-group "[6.07] divide/775acf"
    (test
      "6.07.01: divide/775acf/0.2"
      0
      (compare-image 'divide "775acf" "0.2"))
    (test
      "6.07.02: divide/775acf/0.43"
      0
      (compare-image 'divide "775acf" "0.43"))
    (test
      "6.07.03: divide/775acf/0.6"
      0
      (compare-image 'divide "775acf" "0.6"))
    (test
      "6.07.04: divide/775acf/0.78"
      0
      (compare-image 'divide "775acf" "0.78"))
    (test
      "6.07.05: divide/775acf/1.0"
      0
      (compare-image 'divide "775acf" "1.0"))
  )
  (test-group "[6.08] divide/7f7d0a"
    (test
      "6.08.01: divide/7f7d0a/0.2"
      0
      (compare-image 'divide "7f7d0a" "0.2"))
    (test
      "6.08.02: divide/7f7d0a/0.43"
      0
      (compare-image 'divide "7f7d0a" "0.43"))
    (test
      "6.08.03: divide/7f7d0a/0.6"
      0
      (compare-image 'divide "7f7d0a" "0.6"))
    (test
      "6.08.04: divide/7f7d0a/0.78"
      0
      (compare-image 'divide "7f7d0a" "0.78"))
    (test
      "6.08.05: divide/7f7d0a/1.0"
      0
      (compare-image 'divide "7f7d0a" "1.0"))
  )
  (test-group "[6.09] divide/808080"
    (test
      "6.09.01: divide/808080/0.2"
      0
      (compare-image 'divide "808080" "0.2"))
    (test
      "6.09.02: divide/808080/0.43"
      0
      (compare-image 'divide "808080" "0.43"))
    (test
      "6.09.03: divide/808080/0.6"
      0
      (compare-image 'divide "808080" "0.6"))
    (test
      "6.09.04: divide/808080/0.78"
      0
      (compare-image 'divide "808080" "0.78"))
    (test
      "6.09.05: divide/808080/1.0"
      0
      (compare-image 'divide "808080" "1.0"))
  )
  (test-group "[6.10] divide/899675"
    (test
      "6.10.01: divide/899675/0.2"
      0
      (compare-image 'divide "899675" "0.2"))
    (test
      "6.10.02: divide/899675/0.43"
      0
      (compare-image 'divide "899675" "0.43"))
    (test
      "6.10.03: divide/899675/0.6"
      0
      (compare-image 'divide "899675" "0.6"))
    (test
      "6.10.04: divide/899675/0.78"
      0
      (compare-image 'divide "899675" "0.78"))
    (test
      "6.10.05: divide/899675/1.0"
      0
      (compare-image 'divide "899675" "1.0"))
  )
  (test-group "[6.11] divide/98d5e4"
    (test
      "6.11.01: divide/98d5e4/0.2"
      0
      (compare-image 'divide "98d5e4" "0.2"))
    (test
      "6.11.02: divide/98d5e4/0.43"
      0
      (compare-image 'divide "98d5e4" "0.43"))
    (test
      "6.11.03: divide/98d5e4/0.6"
      0
      (compare-image 'divide "98d5e4" "0.6"))
    (test
      "6.11.04: divide/98d5e4/0.78"
      0
      (compare-image 'divide "98d5e4" "0.78"))
    (test
      "6.11.05: divide/98d5e4/1.0"
      0
      (compare-image 'divide "98d5e4" "1.0"))
  )
  (test-group "[6.12] divide/b5b5b5"
    (test
      "6.12.01: divide/b5b5b5/0.2"
      0
      (compare-image 'divide "b5b5b5" "0.2"))
    (test
      "6.12.02: divide/b5b5b5/0.43"
      0
      (compare-image 'divide "b5b5b5" "0.43"))
    (test
      "6.12.03: divide/b5b5b5/0.6"
      0
      (compare-image 'divide "b5b5b5" "0.6"))
    (test
      "6.12.04: divide/b5b5b5/0.78"
      0
      (compare-image 'divide "b5b5b5" "0.78"))
    (test
      "6.12.05: divide/b5b5b5/1.0"
      0
      (compare-image 'divide "b5b5b5" "1.0"))
  )
  (test-group "[6.13] divide/cd1f3c"
    (test
      "6.13.01: divide/cd1f3c/0.2"
      0
      (compare-image 'divide "cd1f3c" "0.2"))
    (test
      "6.13.02: divide/cd1f3c/0.43"
      0
      (compare-image 'divide "cd1f3c" "0.43"))
    (test
      "6.13.03: divide/cd1f3c/0.6"
      0
      (compare-image 'divide "cd1f3c" "0.6"))
    (test
      "6.13.04: divide/cd1f3c/0.78"
      0
      (compare-image 'divide "cd1f3c" "0.78"))
    (test
      "6.13.05: divide/cd1f3c/1.0"
      0
      (compare-image 'divide "cd1f3c" "1.0"))
  )
  (test-group "[6.14] divide/f62db6"
    (test
      "6.14.01: divide/f62db6/0.2"
      0
      (compare-image 'divide "f62db6" "0.2"))
    (test
      "6.14.02: divide/f62db6/0.43"
      0
      (compare-image 'divide "f62db6" "0.43"))
    (test
      "6.14.03: divide/f62db6/0.6"
      0
      (compare-image 'divide "f62db6" "0.6"))
    (test
      "6.14.04: divide/f62db6/0.78"
      0
      (compare-image 'divide "f62db6" "0.78"))
    (test
      "6.14.05: divide/f62db6/1.0"
      0
      (compare-image 'divide "f62db6" "1.0"))
  )
  (test-group "[6.15] divide/ff0000"
    (test
      "6.15.01: divide/ff0000/0.2"
      0
      (compare-image 'divide "ff0000" "0.2"))
    (test
      "6.15.02: divide/ff0000/0.43"
      0
      (compare-image 'divide "ff0000" "0.43"))
    (test
      "6.15.03: divide/ff0000/0.6"
      0
      (compare-image 'divide "ff0000" "0.6"))
    (test
      "6.15.04: divide/ff0000/0.78"
      0
      (compare-image 'divide "ff0000" "0.78"))
    (test
      "6.15.05: divide/ff0000/1.0"
      0
      (compare-image 'divide "ff0000" "1.0"))
  )
  (test-group "[6.16] divide/ffffff"
    (test
      "6.16.01: divide/ffffff/0.2"
      0
      (compare-image 'divide "ffffff" "0.2"))
    (test
      "6.16.02: divide/ffffff/0.43"
      0
      (compare-image 'divide "ffffff" "0.43"))
    (test
      "6.16.03: divide/ffffff/0.6"
      0
      (compare-image 'divide "ffffff" "0.6"))
    (test
      "6.16.04: divide/ffffff/0.78"
      0
      (compare-image 'divide "ffffff" "0.78"))
    (test
      "6.16.05: divide/ffffff/1.0"
      0
      (compare-image 'divide "ffffff" "1.0"))
  )
)
(test-group "[7] dodge"
  (test-group "[7.01] dodge/000000"
    (test
      "7.01.01: dodge/000000/0.2"
      0
      (compare-image 'dodge "000000" "0.2"))
    (test
      "7.01.02: dodge/000000/0.43"
      0
      (compare-image 'dodge "000000" "0.43"))
    (test
      "7.01.03: dodge/000000/0.6"
      0
      (compare-image 'dodge "000000" "0.6"))
    (test
      "7.01.04: dodge/000000/0.78"
      0
      (compare-image 'dodge "000000" "0.78"))
    (test
      "7.01.05: dodge/000000/1.0"
      0
      (compare-image 'dodge "000000" "1.0"))
  )
  (test-group "[7.02] dodge/0000ff"
    (test
      "7.02.01: dodge/0000ff/0.2"
      0
      (compare-image 'dodge "0000ff" "0.2"))
    (test
      "7.02.02: dodge/0000ff/0.43"
      0
      (compare-image 'dodge "0000ff" "0.43"))
    (test
      "7.02.03: dodge/0000ff/0.6"
      0
      (compare-image 'dodge "0000ff" "0.6"))
    (test
      "7.02.04: dodge/0000ff/0.78"
      0
      (compare-image 'dodge "0000ff" "0.78"))
    (test
      "7.02.05: dodge/0000ff/1.0"
      0
      (compare-image 'dodge "0000ff" "1.0"))
  )
  (test-group "[7.03] dodge/00ff00"
    (test
      "7.03.01: dodge/00ff00/0.2"
      0
      (compare-image 'dodge "00ff00" "0.2"))
    (test
      "7.03.02: dodge/00ff00/0.43"
      0
      (compare-image 'dodge "00ff00" "0.43"))
    (test
      "7.03.03: dodge/00ff00/0.6"
      0
      (compare-image 'dodge "00ff00" "0.6"))
    (test
      "7.03.04: dodge/00ff00/0.78"
      0
      (compare-image 'dodge "00ff00" "0.78"))
    (test
      "7.03.05: dodge/00ff00/1.0"
      0
      (compare-image 'dodge "00ff00" "1.0"))
  )
  (test-group "[7.04] dodge/27249c"
    (test
      "7.04.01: dodge/27249c/0.2"
      0
      (compare-image 'dodge "27249c" "0.2"))
    (test
      "7.04.02: dodge/27249c/0.43"
      0
      (compare-image 'dodge "27249c" "0.43"))
    (test
      "7.04.03: dodge/27249c/0.6"
      0
      (compare-image 'dodge "27249c" "0.6"))
    (test
      "7.04.04: dodge/27249c/0.78"
      0
      (compare-image 'dodge "27249c" "0.78"))
    (test
      "7.04.05: dodge/27249c/1.0"
      0
      (compare-image 'dodge "27249c" "1.0"))
  )
  (test-group "[7.05] dodge/54fa0d"
    (test
      "7.05.01: dodge/54fa0d/0.2"
      0
      (compare-image 'dodge "54fa0d" "0.2"))
    (test
      "7.05.02: dodge/54fa0d/0.43"
      0
      (compare-image 'dodge "54fa0d" "0.43"))
    (test
      "7.05.03: dodge/54fa0d/0.6"
      0
      (compare-image 'dodge "54fa0d" "0.6"))
    (test
      "7.05.04: dodge/54fa0d/0.78"
      0
      (compare-image 'dodge "54fa0d" "0.78"))
    (test
      "7.05.05: dodge/54fa0d/1.0"
      0
      (compare-image 'dodge "54fa0d" "1.0"))
  )
  (test-group "[7.06] dodge/5c5c5c"
    (test
      "7.06.01: dodge/5c5c5c/0.2"
      0
      (compare-image 'dodge "5c5c5c" "0.2"))
    (test
      "7.06.02: dodge/5c5c5c/0.43"
      0
      (compare-image 'dodge "5c5c5c" "0.43"))
    (test
      "7.06.03: dodge/5c5c5c/0.6"
      0
      (compare-image 'dodge "5c5c5c" "0.6"))
    (test
      "7.06.04: dodge/5c5c5c/0.78"
      0
      (compare-image 'dodge "5c5c5c" "0.78"))
    (test
      "7.06.05: dodge/5c5c5c/1.0"
      0
      (compare-image 'dodge "5c5c5c" "1.0"))
  )
  (test-group "[7.07] dodge/775acf"
    (test
      "7.07.01: dodge/775acf/0.2"
      0
      (compare-image 'dodge "775acf" "0.2"))
    (test
      "7.07.02: dodge/775acf/0.43"
      0
      (compare-image 'dodge "775acf" "0.43"))
    (test
      "7.07.03: dodge/775acf/0.6"
      0
      (compare-image 'dodge "775acf" "0.6"))
    (test
      "7.07.04: dodge/775acf/0.78"
      0
      (compare-image 'dodge "775acf" "0.78"))
    (test
      "7.07.05: dodge/775acf/1.0"
      0
      (compare-image 'dodge "775acf" "1.0"))
  )
  (test-group "[7.08] dodge/7f7d0a"
    (test
      "7.08.01: dodge/7f7d0a/0.2"
      0
      (compare-image 'dodge "7f7d0a" "0.2"))
    (test
      "7.08.02: dodge/7f7d0a/0.43"
      0
      (compare-image 'dodge "7f7d0a" "0.43"))
    (test
      "7.08.03: dodge/7f7d0a/0.6"
      0
      (compare-image 'dodge "7f7d0a" "0.6"))
    (test
      "7.08.04: dodge/7f7d0a/0.78"
      0
      (compare-image 'dodge "7f7d0a" "0.78"))
    (test
      "7.08.05: dodge/7f7d0a/1.0"
      0
      (compare-image 'dodge "7f7d0a" "1.0"))
  )
  (test-group "[7.09] dodge/808080"
    (test
      "7.09.01: dodge/808080/0.2"
      0
      (compare-image 'dodge "808080" "0.2"))
    (test
      "7.09.02: dodge/808080/0.43"
      0
      (compare-image 'dodge "808080" "0.43"))
    (test
      "7.09.03: dodge/808080/0.6"
      0
      (compare-image 'dodge "808080" "0.6"))
    (test
      "7.09.04: dodge/808080/0.78"
      0
      (compare-image 'dodge "808080" "0.78"))
    (test
      "7.09.05: dodge/808080/1.0"
      0
      (compare-image 'dodge "808080" "1.0"))
  )
  (test-group "[7.10] dodge/899675"
    (test
      "7.10.01: dodge/899675/0.2"
      0
      (compare-image 'dodge "899675" "0.2"))
    (test
      "7.10.02: dodge/899675/0.43"
      0
      (compare-image 'dodge "899675" "0.43"))
    (test
      "7.10.03: dodge/899675/0.6"
      0
      (compare-image 'dodge "899675" "0.6"))
    (test
      "7.10.04: dodge/899675/0.78"
      0
      (compare-image 'dodge "899675" "0.78"))
    (test
      "7.10.05: dodge/899675/1.0"
      0
      (compare-image 'dodge "899675" "1.0"))
  )
  (test-group "[7.11] dodge/98d5e4"
    (test
      "7.11.01: dodge/98d5e4/0.2"
      0
      (compare-image 'dodge "98d5e4" "0.2"))
    (test
      "7.11.02: dodge/98d5e4/0.43"
      0
      (compare-image 'dodge "98d5e4" "0.43"))
    (test
      "7.11.03: dodge/98d5e4/0.6"
      0
      (compare-image 'dodge "98d5e4" "0.6"))
    (test
      "7.11.04: dodge/98d5e4/0.78"
      0
      (compare-image 'dodge "98d5e4" "0.78"))
    (test
      "7.11.05: dodge/98d5e4/1.0"
      0
      (compare-image 'dodge "98d5e4" "1.0"))
  )
  (test-group "[7.12] dodge/b5b5b5"
    (test
      "7.12.01: dodge/b5b5b5/0.2"
      0
      (compare-image 'dodge "b5b5b5" "0.2"))
    (test
      "7.12.02: dodge/b5b5b5/0.43"
      0
      (compare-image 'dodge "b5b5b5" "0.43"))
    (test
      "7.12.03: dodge/b5b5b5/0.6"
      0
      (compare-image 'dodge "b5b5b5" "0.6"))
    (test
      "7.12.04: dodge/b5b5b5/0.78"
      0
      (compare-image 'dodge "b5b5b5" "0.78"))
    (test
      "7.12.05: dodge/b5b5b5/1.0"
      0
      (compare-image 'dodge "b5b5b5" "1.0"))
  )
  (test-group "[7.13] dodge/cd1f3c"
    (test
      "7.13.01: dodge/cd1f3c/0.2"
      0
      (compare-image 'dodge "cd1f3c" "0.2"))
    (test
      "7.13.02: dodge/cd1f3c/0.43"
      0
      (compare-image 'dodge "cd1f3c" "0.43"))
    (test
      "7.13.03: dodge/cd1f3c/0.6"
      0
      (compare-image 'dodge "cd1f3c" "0.6"))
    (test
      "7.13.04: dodge/cd1f3c/0.78"
      0
      (compare-image 'dodge "cd1f3c" "0.78"))
    (test
      "7.13.05: dodge/cd1f3c/1.0"
      0
      (compare-image 'dodge "cd1f3c" "1.0"))
  )
  (test-group "[7.14] dodge/f62db6"
    (test
      "7.14.01: dodge/f62db6/0.2"
      0
      (compare-image 'dodge "f62db6" "0.2"))
    (test
      "7.14.02: dodge/f62db6/0.43"
      0
      (compare-image 'dodge "f62db6" "0.43"))
    (test
      "7.14.03: dodge/f62db6/0.6"
      0
      (compare-image 'dodge "f62db6" "0.6"))
    (test
      "7.14.04: dodge/f62db6/0.78"
      0
      (compare-image 'dodge "f62db6" "0.78"))
    (test
      "7.14.05: dodge/f62db6/1.0"
      0
      (compare-image 'dodge "f62db6" "1.0"))
  )
  (test-group "[7.15] dodge/ff0000"
    (test
      "7.15.01: dodge/ff0000/0.2"
      0
      (compare-image 'dodge "ff0000" "0.2"))
    (test
      "7.15.02: dodge/ff0000/0.43"
      0
      (compare-image 'dodge "ff0000" "0.43"))
    (test
      "7.15.03: dodge/ff0000/0.6"
      0
      (compare-image 'dodge "ff0000" "0.6"))
    (test
      "7.15.04: dodge/ff0000/0.78"
      0
      (compare-image 'dodge "ff0000" "0.78"))
    (test
      "7.15.05: dodge/ff0000/1.0"
      0
      (compare-image 'dodge "ff0000" "1.0"))
  )
  (test-group "[7.16] dodge/ffffff"
    (test
      "7.16.01: dodge/ffffff/0.2"
      0
      (compare-image 'dodge "ffffff" "0.2"))
    (test
      "7.16.02: dodge/ffffff/0.43"
      0
      (compare-image 'dodge "ffffff" "0.43"))
    (test
      "7.16.03: dodge/ffffff/0.6"
      0
      (compare-image 'dodge "ffffff" "0.6"))
    (test
      "7.16.04: dodge/ffffff/0.78"
      0
      (compare-image 'dodge "ffffff" "0.78"))
    (test
      "7.16.05: dodge/ffffff/1.0"
      0
      (compare-image 'dodge "ffffff" "1.0"))
  )
)
(test-group "[8] grain-extract"
  (test-group "[8.01] grain-extract/000000"
    (test
      "8.01.01: grain-extract/000000/0.2"
      0
      (compare-image 'grain-extract "000000" "0.2"))
    (test
      "8.01.02: grain-extract/000000/0.43"
      0
      (compare-image 'grain-extract "000000" "0.43"))
    (test
      "8.01.03: grain-extract/000000/0.6"
      0
      (compare-image 'grain-extract "000000" "0.6"))
    (test
      "8.01.04: grain-extract/000000/0.78"
      0
      (compare-image 'grain-extract "000000" "0.78"))
    (test
      "8.01.05: grain-extract/000000/1.0"
      0
      (compare-image 'grain-extract "000000" "1.0"))
  )
  (test-group "[8.02] grain-extract/0000ff"
    (test
      "8.02.01: grain-extract/0000ff/0.2"
      0
      (compare-image 'grain-extract "0000ff" "0.2"))
    (test
      "8.02.02: grain-extract/0000ff/0.43"
      0
      (compare-image 'grain-extract "0000ff" "0.43"))
    (test
      "8.02.03: grain-extract/0000ff/0.6"
      0
      (compare-image 'grain-extract "0000ff" "0.6"))
    (test
      "8.02.04: grain-extract/0000ff/0.78"
      0
      (compare-image 'grain-extract "0000ff" "0.78"))
    (test
      "8.02.05: grain-extract/0000ff/1.0"
      0
      (compare-image 'grain-extract "0000ff" "1.0"))
  )
  (test-group "[8.03] grain-extract/00ff00"
    (test
      "8.03.01: grain-extract/00ff00/0.2"
      0
      (compare-image 'grain-extract "00ff00" "0.2"))
    (test
      "8.03.02: grain-extract/00ff00/0.43"
      0
      (compare-image 'grain-extract "00ff00" "0.43"))
    (test
      "8.03.03: grain-extract/00ff00/0.6"
      0
      (compare-image 'grain-extract "00ff00" "0.6"))
    (test
      "8.03.04: grain-extract/00ff00/0.78"
      0
      (compare-image 'grain-extract "00ff00" "0.78"))
    (test
      "8.03.05: grain-extract/00ff00/1.0"
      0
      (compare-image 'grain-extract "00ff00" "1.0"))
  )
  (test-group "[8.04] grain-extract/27249c"
    (test
      "8.04.01: grain-extract/27249c/0.2"
      0
      (compare-image 'grain-extract "27249c" "0.2"))
    (test
      "8.04.02: grain-extract/27249c/0.43"
      0
      (compare-image 'grain-extract "27249c" "0.43"))
    (test
      "8.04.03: grain-extract/27249c/0.6"
      0
      (compare-image 'grain-extract "27249c" "0.6"))
    (test
      "8.04.04: grain-extract/27249c/0.78"
      0
      (compare-image 'grain-extract "27249c" "0.78"))
    (test
      "8.04.05: grain-extract/27249c/1.0"
      0
      (compare-image 'grain-extract "27249c" "1.0"))
  )
  (test-group "[8.05] grain-extract/54fa0d"
    (test
      "8.05.01: grain-extract/54fa0d/0.2"
      0
      (compare-image 'grain-extract "54fa0d" "0.2"))
    (test
      "8.05.02: grain-extract/54fa0d/0.43"
      0
      (compare-image 'grain-extract "54fa0d" "0.43"))
    (test
      "8.05.03: grain-extract/54fa0d/0.6"
      0
      (compare-image 'grain-extract "54fa0d" "0.6"))
    (test
      "8.05.04: grain-extract/54fa0d/0.78"
      0
      (compare-image 'grain-extract "54fa0d" "0.78"))
    (test
      "8.05.05: grain-extract/54fa0d/1.0"
      0
      (compare-image 'grain-extract "54fa0d" "1.0"))
  )
  (test-group "[8.06] grain-extract/5c5c5c"
    (test
      "8.06.01: grain-extract/5c5c5c/0.2"
      0
      (compare-image 'grain-extract "5c5c5c" "0.2"))
    (test
      "8.06.02: grain-extract/5c5c5c/0.43"
      0
      (compare-image 'grain-extract "5c5c5c" "0.43"))
    (test
      "8.06.03: grain-extract/5c5c5c/0.6"
      0
      (compare-image 'grain-extract "5c5c5c" "0.6"))
    (test
      "8.06.04: grain-extract/5c5c5c/0.78"
      0
      (compare-image 'grain-extract "5c5c5c" "0.78"))
    (test
      "8.06.05: grain-extract/5c5c5c/1.0"
      0
      (compare-image 'grain-extract "5c5c5c" "1.0"))
  )
  (test-group "[8.07] grain-extract/775acf"
    (test
      "8.07.01: grain-extract/775acf/0.2"
      0
      (compare-image 'grain-extract "775acf" "0.2"))
    (test
      "8.07.02: grain-extract/775acf/0.43"
      0
      (compare-image 'grain-extract "775acf" "0.43"))
    (test
      "8.07.03: grain-extract/775acf/0.6"
      0
      (compare-image 'grain-extract "775acf" "0.6"))
    (test
      "8.07.04: grain-extract/775acf/0.78"
      0
      (compare-image 'grain-extract "775acf" "0.78"))
    (test
      "8.07.05: grain-extract/775acf/1.0"
      0
      (compare-image 'grain-extract "775acf" "1.0"))
  )
  (test-group "[8.08] grain-extract/7f7d0a"
    (test
      "8.08.01: grain-extract/7f7d0a/0.2"
      0
      (compare-image 'grain-extract "7f7d0a" "0.2"))
    (test
      "8.08.02: grain-extract/7f7d0a/0.43"
      0
      (compare-image 'grain-extract "7f7d0a" "0.43"))
    (test
      "8.08.03: grain-extract/7f7d0a/0.6"
      0
      (compare-image 'grain-extract "7f7d0a" "0.6"))
    (test
      "8.08.04: grain-extract/7f7d0a/0.78"
      0
      (compare-image 'grain-extract "7f7d0a" "0.78"))
    (test
      "8.08.05: grain-extract/7f7d0a/1.0"
      0
      (compare-image 'grain-extract "7f7d0a" "1.0"))
  )
  (test-group "[8.09] grain-extract/808080"
    (test
      "8.09.01: grain-extract/808080/0.2"
      0
      (compare-image 'grain-extract "808080" "0.2"))
    (test
      "8.09.02: grain-extract/808080/0.43"
      0
      (compare-image 'grain-extract "808080" "0.43"))
    (test
      "8.09.03: grain-extract/808080/0.6"
      0
      (compare-image 'grain-extract "808080" "0.6"))
    (test
      "8.09.04: grain-extract/808080/0.78"
      0
      (compare-image 'grain-extract "808080" "0.78"))
    (test
      "8.09.05: grain-extract/808080/1.0"
      0
      (compare-image 'grain-extract "808080" "1.0"))
  )
  (test-group "[8.10] grain-extract/899675"
    (test
      "8.10.01: grain-extract/899675/0.2"
      0
      (compare-image 'grain-extract "899675" "0.2"))
    (test
      "8.10.02: grain-extract/899675/0.43"
      0
      (compare-image 'grain-extract "899675" "0.43"))
    (test
      "8.10.03: grain-extract/899675/0.6"
      0
      (compare-image 'grain-extract "899675" "0.6"))
    (test
      "8.10.04: grain-extract/899675/0.78"
      0
      (compare-image 'grain-extract "899675" "0.78"))
    (test
      "8.10.05: grain-extract/899675/1.0"
      0
      (compare-image 'grain-extract "899675" "1.0"))
  )
  (test-group "[8.11] grain-extract/98d5e4"
    (test
      "8.11.01: grain-extract/98d5e4/0.2"
      0
      (compare-image 'grain-extract "98d5e4" "0.2"))
    (test
      "8.11.02: grain-extract/98d5e4/0.43"
      0
      (compare-image 'grain-extract "98d5e4" "0.43"))
    (test
      "8.11.03: grain-extract/98d5e4/0.6"
      0
      (compare-image 'grain-extract "98d5e4" "0.6"))
    (test
      "8.11.04: grain-extract/98d5e4/0.78"
      0
      (compare-image 'grain-extract "98d5e4" "0.78"))
    (test
      "8.11.05: grain-extract/98d5e4/1.0"
      0
      (compare-image 'grain-extract "98d5e4" "1.0"))
  )
  (test-group "[8.12] grain-extract/b5b5b5"
    (test
      "8.12.01: grain-extract/b5b5b5/0.2"
      0
      (compare-image 'grain-extract "b5b5b5" "0.2"))
    (test
      "8.12.02: grain-extract/b5b5b5/0.43"
      0
      (compare-image 'grain-extract "b5b5b5" "0.43"))
    (test
      "8.12.03: grain-extract/b5b5b5/0.6"
      0
      (compare-image 'grain-extract "b5b5b5" "0.6"))
    (test
      "8.12.04: grain-extract/b5b5b5/0.78"
      0
      (compare-image 'grain-extract "b5b5b5" "0.78"))
    (test
      "8.12.05: grain-extract/b5b5b5/1.0"
      0
      (compare-image 'grain-extract "b5b5b5" "1.0"))
  )
  (test-group "[8.13] grain-extract/cd1f3c"
    (test
      "8.13.01: grain-extract/cd1f3c/0.2"
      0
      (compare-image 'grain-extract "cd1f3c" "0.2"))
    (test
      "8.13.02: grain-extract/cd1f3c/0.43"
      0
      (compare-image 'grain-extract "cd1f3c" "0.43"))
    (test
      "8.13.03: grain-extract/cd1f3c/0.6"
      0
      (compare-image 'grain-extract "cd1f3c" "0.6"))
    (test
      "8.13.04: grain-extract/cd1f3c/0.78"
      0
      (compare-image 'grain-extract "cd1f3c" "0.78"))
    (test
      "8.13.05: grain-extract/cd1f3c/1.0"
      0
      (compare-image 'grain-extract "cd1f3c" "1.0"))
  )
  (test-group "[8.14] grain-extract/f62db6"
    (test
      "8.14.01: grain-extract/f62db6/0.2"
      0
      (compare-image 'grain-extract "f62db6" "0.2"))
    (test
      "8.14.02: grain-extract/f62db6/0.43"
      0
      (compare-image 'grain-extract "f62db6" "0.43"))
    (test
      "8.14.03: grain-extract/f62db6/0.6"
      0
      (compare-image 'grain-extract "f62db6" "0.6"))
    (test
      "8.14.04: grain-extract/f62db6/0.78"
      0
      (compare-image 'grain-extract "f62db6" "0.78"))
    (test
      "8.14.05: grain-extract/f62db6/1.0"
      0
      (compare-image 'grain-extract "f62db6" "1.0"))
  )
  (test-group "[8.15] grain-extract/ff0000"
    (test
      "8.15.01: grain-extract/ff0000/0.2"
      0
      (compare-image 'grain-extract "ff0000" "0.2"))
    (test
      "8.15.02: grain-extract/ff0000/0.43"
      0
      (compare-image 'grain-extract "ff0000" "0.43"))
    (test
      "8.15.03: grain-extract/ff0000/0.6"
      0
      (compare-image 'grain-extract "ff0000" "0.6"))
    (test
      "8.15.04: grain-extract/ff0000/0.78"
      0
      (compare-image 'grain-extract "ff0000" "0.78"))
    (test
      "8.15.05: grain-extract/ff0000/1.0"
      0
      (compare-image 'grain-extract "ff0000" "1.0"))
  )
  (test-group "[8.16] grain-extract/ffffff"
    (test
      "8.16.01: grain-extract/ffffff/0.2"
      0
      (compare-image 'grain-extract "ffffff" "0.2"))
    (test
      "8.16.02: grain-extract/ffffff/0.43"
      0
      (compare-image 'grain-extract "ffffff" "0.43"))
    (test
      "8.16.03: grain-extract/ffffff/0.6"
      0
      (compare-image 'grain-extract "ffffff" "0.6"))
    (test
      "8.16.04: grain-extract/ffffff/0.78"
      0
      (compare-image 'grain-extract "ffffff" "0.78"))
    (test
      "8.16.05: grain-extract/ffffff/1.0"
      0
      (compare-image 'grain-extract "ffffff" "1.0"))
  )
)
(test-group "[9] grain-merge"
  (test-group "[9.01] grain-merge/000000"
    (test
      "9.01.01: grain-merge/000000/0.2"
      0
      (compare-image 'grain-merge "000000" "0.2"))
    (test
      "9.01.02: grain-merge/000000/0.43"
      0
      (compare-image 'grain-merge "000000" "0.43"))
    (test
      "9.01.03: grain-merge/000000/0.6"
      0
      (compare-image 'grain-merge "000000" "0.6"))
    (test
      "9.01.04: grain-merge/000000/0.78"
      0
      (compare-image 'grain-merge "000000" "0.78"))
    (test
      "9.01.05: grain-merge/000000/1.0"
      0
      (compare-image 'grain-merge "000000" "1.0"))
  )
  (test-group "[9.02] grain-merge/0000ff"
    (test
      "9.02.01: grain-merge/0000ff/0.2"
      0
      (compare-image 'grain-merge "0000ff" "0.2"))
    (test
      "9.02.02: grain-merge/0000ff/0.43"
      0
      (compare-image 'grain-merge "0000ff" "0.43"))
    (test
      "9.02.03: grain-merge/0000ff/0.6"
      0
      (compare-image 'grain-merge "0000ff" "0.6"))
    (test
      "9.02.04: grain-merge/0000ff/0.78"
      0
      (compare-image 'grain-merge "0000ff" "0.78"))
    (test
      "9.02.05: grain-merge/0000ff/1.0"
      0
      (compare-image 'grain-merge "0000ff" "1.0"))
  )
  (test-group "[9.03] grain-merge/00ff00"
    (test
      "9.03.01: grain-merge/00ff00/0.2"
      0
      (compare-image 'grain-merge "00ff00" "0.2"))
    (test
      "9.03.02: grain-merge/00ff00/0.43"
      0
      (compare-image 'grain-merge "00ff00" "0.43"))
    (test
      "9.03.03: grain-merge/00ff00/0.6"
      0
      (compare-image 'grain-merge "00ff00" "0.6"))
    (test
      "9.03.04: grain-merge/00ff00/0.78"
      0
      (compare-image 'grain-merge "00ff00" "0.78"))
    (test
      "9.03.05: grain-merge/00ff00/1.0"
      0
      (compare-image 'grain-merge "00ff00" "1.0"))
  )
  (test-group "[9.04] grain-merge/27249c"
    (test
      "9.04.01: grain-merge/27249c/0.2"
      0
      (compare-image 'grain-merge "27249c" "0.2"))
    (test
      "9.04.02: grain-merge/27249c/0.43"
      0
      (compare-image 'grain-merge "27249c" "0.43"))
    (test
      "9.04.03: grain-merge/27249c/0.6"
      0
      (compare-image 'grain-merge "27249c" "0.6"))
    (test
      "9.04.04: grain-merge/27249c/0.78"
      0
      (compare-image 'grain-merge "27249c" "0.78"))
    (test
      "9.04.05: grain-merge/27249c/1.0"
      0
      (compare-image 'grain-merge "27249c" "1.0"))
  )
  (test-group "[9.05] grain-merge/54fa0d"
    (test
      "9.05.01: grain-merge/54fa0d/0.2"
      0
      (compare-image 'grain-merge "54fa0d" "0.2"))
    (test
      "9.05.02: grain-merge/54fa0d/0.43"
      0
      (compare-image 'grain-merge "54fa0d" "0.43"))
    (test
      "9.05.03: grain-merge/54fa0d/0.6"
      0
      (compare-image 'grain-merge "54fa0d" "0.6"))
    (test
      "9.05.04: grain-merge/54fa0d/0.78"
      0
      (compare-image 'grain-merge "54fa0d" "0.78"))
    (test
      "9.05.05: grain-merge/54fa0d/1.0"
      0
      (compare-image 'grain-merge "54fa0d" "1.0"))
  )
  (test-group "[9.06] grain-merge/5c5c5c"
    (test
      "9.06.01: grain-merge/5c5c5c/0.2"
      0
      (compare-image 'grain-merge "5c5c5c" "0.2"))
    (test
      "9.06.02: grain-merge/5c5c5c/0.43"
      0
      (compare-image 'grain-merge "5c5c5c" "0.43"))
    (test
      "9.06.03: grain-merge/5c5c5c/0.6"
      0
      (compare-image 'grain-merge "5c5c5c" "0.6"))
    (test
      "9.06.04: grain-merge/5c5c5c/0.78"
      0
      (compare-image 'grain-merge "5c5c5c" "0.78"))
    (test
      "9.06.05: grain-merge/5c5c5c/1.0"
      0
      (compare-image 'grain-merge "5c5c5c" "1.0"))
  )
  (test-group "[9.07] grain-merge/775acf"
    (test
      "9.07.01: grain-merge/775acf/0.2"
      0
      (compare-image 'grain-merge "775acf" "0.2"))
    (test
      "9.07.02: grain-merge/775acf/0.43"
      0
      (compare-image 'grain-merge "775acf" "0.43"))
    (test
      "9.07.03: grain-merge/775acf/0.6"
      0
      (compare-image 'grain-merge "775acf" "0.6"))
    (test
      "9.07.04: grain-merge/775acf/0.78"
      0
      (compare-image 'grain-merge "775acf" "0.78"))
    (test
      "9.07.05: grain-merge/775acf/1.0"
      0
      (compare-image 'grain-merge "775acf" "1.0"))
  )
  (test-group "[9.08] grain-merge/7f7d0a"
    (test
      "9.08.01: grain-merge/7f7d0a/0.2"
      0
      (compare-image 'grain-merge "7f7d0a" "0.2"))
    (test
      "9.08.02: grain-merge/7f7d0a/0.43"
      0
      (compare-image 'grain-merge "7f7d0a" "0.43"))
    (test
      "9.08.03: grain-merge/7f7d0a/0.6"
      0
      (compare-image 'grain-merge "7f7d0a" "0.6"))
    (test
      "9.08.04: grain-merge/7f7d0a/0.78"
      0
      (compare-image 'grain-merge "7f7d0a" "0.78"))
    (test
      "9.08.05: grain-merge/7f7d0a/1.0"
      0
      (compare-image 'grain-merge "7f7d0a" "1.0"))
  )
  (test-group "[9.09] grain-merge/808080"
    (test
      "9.09.01: grain-merge/808080/0.2"
      0
      (compare-image 'grain-merge "808080" "0.2"))
    (test
      "9.09.02: grain-merge/808080/0.43"
      0
      (compare-image 'grain-merge "808080" "0.43"))
    (test
      "9.09.03: grain-merge/808080/0.6"
      0
      (compare-image 'grain-merge "808080" "0.6"))
    (test
      "9.09.04: grain-merge/808080/0.78"
      0
      (compare-image 'grain-merge "808080" "0.78"))
    (test
      "9.09.05: grain-merge/808080/1.0"
      0
      (compare-image 'grain-merge "808080" "1.0"))
  )
  (test-group "[9.10] grain-merge/899675"
    (test
      "9.10.01: grain-merge/899675/0.2"
      0
      (compare-image 'grain-merge "899675" "0.2"))
    (test
      "9.10.02: grain-merge/899675/0.43"
      0
      (compare-image 'grain-merge "899675" "0.43"))
    (test
      "9.10.03: grain-merge/899675/0.6"
      0
      (compare-image 'grain-merge "899675" "0.6"))
    (test
      "9.10.04: grain-merge/899675/0.78"
      0
      (compare-image 'grain-merge "899675" "0.78"))
    (test
      "9.10.05: grain-merge/899675/1.0"
      0
      (compare-image 'grain-merge "899675" "1.0"))
  )
  (test-group "[9.11] grain-merge/98d5e4"
    (test
      "9.11.01: grain-merge/98d5e4/0.2"
      0
      (compare-image 'grain-merge "98d5e4" "0.2"))
    (test
      "9.11.02: grain-merge/98d5e4/0.43"
      0
      (compare-image 'grain-merge "98d5e4" "0.43"))
    (test
      "9.11.03: grain-merge/98d5e4/0.6"
      0
      (compare-image 'grain-merge "98d5e4" "0.6"))
    (test
      "9.11.04: grain-merge/98d5e4/0.78"
      0
      (compare-image 'grain-merge "98d5e4" "0.78"))
    (test
      "9.11.05: grain-merge/98d5e4/1.0"
      0
      (compare-image 'grain-merge "98d5e4" "1.0"))
  )
  (test-group "[9.12] grain-merge/b5b5b5"
    (test
      "9.12.01: grain-merge/b5b5b5/0.2"
      0
      (compare-image 'grain-merge "b5b5b5" "0.2"))
    (test
      "9.12.02: grain-merge/b5b5b5/0.43"
      0
      (compare-image 'grain-merge "b5b5b5" "0.43"))
    (test
      "9.12.03: grain-merge/b5b5b5/0.6"
      0
      (compare-image 'grain-merge "b5b5b5" "0.6"))
    (test
      "9.12.04: grain-merge/b5b5b5/0.78"
      0
      (compare-image 'grain-merge "b5b5b5" "0.78"))
    (test
      "9.12.05: grain-merge/b5b5b5/1.0"
      0
      (compare-image 'grain-merge "b5b5b5" "1.0"))
  )
  (test-group "[9.13] grain-merge/cd1f3c"
    (test
      "9.13.01: grain-merge/cd1f3c/0.2"
      0
      (compare-image 'grain-merge "cd1f3c" "0.2"))
    (test
      "9.13.02: grain-merge/cd1f3c/0.43"
      0
      (compare-image 'grain-merge "cd1f3c" "0.43"))
    (test
      "9.13.03: grain-merge/cd1f3c/0.6"
      0
      (compare-image 'grain-merge "cd1f3c" "0.6"))
    (test
      "9.13.04: grain-merge/cd1f3c/0.78"
      0
      (compare-image 'grain-merge "cd1f3c" "0.78"))
    (test
      "9.13.05: grain-merge/cd1f3c/1.0"
      0
      (compare-image 'grain-merge "cd1f3c" "1.0"))
  )
  (test-group "[9.14] grain-merge/f62db6"
    (test
      "9.14.01: grain-merge/f62db6/0.2"
      0
      (compare-image 'grain-merge "f62db6" "0.2"))
    (test
      "9.14.02: grain-merge/f62db6/0.43"
      0
      (compare-image 'grain-merge "f62db6" "0.43"))
    (test
      "9.14.03: grain-merge/f62db6/0.6"
      0
      (compare-image 'grain-merge "f62db6" "0.6"))
    (test
      "9.14.04: grain-merge/f62db6/0.78"
      0
      (compare-image 'grain-merge "f62db6" "0.78"))
    (test
      "9.14.05: grain-merge/f62db6/1.0"
      0
      (compare-image 'grain-merge "f62db6" "1.0"))
  )
  (test-group "[9.15] grain-merge/ff0000"
    (test
      "9.15.01: grain-merge/ff0000/0.2"
      0
      (compare-image 'grain-merge "ff0000" "0.2"))
    (test
      "9.15.02: grain-merge/ff0000/0.43"
      0
      (compare-image 'grain-merge "ff0000" "0.43"))
    (test
      "9.15.03: grain-merge/ff0000/0.6"
      0
      (compare-image 'grain-merge "ff0000" "0.6"))
    (test
      "9.15.04: grain-merge/ff0000/0.78"
      0
      (compare-image 'grain-merge "ff0000" "0.78"))
    (test
      "9.15.05: grain-merge/ff0000/1.0"
      0
      (compare-image 'grain-merge "ff0000" "1.0"))
  )
  (test-group "[9.16] grain-merge/ffffff"
    (test
      "9.16.01: grain-merge/ffffff/0.2"
      0
      (compare-image 'grain-merge "ffffff" "0.2"))
    (test
      "9.16.02: grain-merge/ffffff/0.43"
      0
      (compare-image 'grain-merge "ffffff" "0.43"))
    (test
      "9.16.03: grain-merge/ffffff/0.6"
      0
      (compare-image 'grain-merge "ffffff" "0.6"))
    (test
      "9.16.04: grain-merge/ffffff/0.78"
      0
      (compare-image 'grain-merge "ffffff" "0.78"))
    (test
      "9.16.05: grain-merge/ffffff/1.0"
      0
      (compare-image 'grain-merge "ffffff" "1.0"))
  )
)
(test-group "[10] hard-light"
  (test-group "[10.01] hard-light/000000"
    (test
      "10.01.01: hard-light/000000/0.2"
      0
      (compare-image 'hard-light "000000" "0.2"))
    (test
      "10.01.02: hard-light/000000/0.43"
      0
      (compare-image 'hard-light "000000" "0.43"))
    (test
      "10.01.03: hard-light/000000/0.6"
      0
      (compare-image 'hard-light "000000" "0.6"))
    (test
      "10.01.04: hard-light/000000/0.78"
      0
      (compare-image 'hard-light "000000" "0.78"))
    (test
      "10.01.05: hard-light/000000/1.0"
      0
      (compare-image 'hard-light "000000" "1.0"))
  )
  (test-group "[10.02] hard-light/0000ff"
    (test
      "10.02.01: hard-light/0000ff/0.2"
      0
      (compare-image 'hard-light "0000ff" "0.2"))
    (test
      "10.02.02: hard-light/0000ff/0.43"
      0
      (compare-image 'hard-light "0000ff" "0.43"))
    (test
      "10.02.03: hard-light/0000ff/0.6"
      0
      (compare-image 'hard-light "0000ff" "0.6"))
    (test
      "10.02.04: hard-light/0000ff/0.78"
      0
      (compare-image 'hard-light "0000ff" "0.78"))
    (test
      "10.02.05: hard-light/0000ff/1.0"
      0
      (compare-image 'hard-light "0000ff" "1.0"))
  )
  (test-group "[10.03] hard-light/00ff00"
    (test
      "10.03.01: hard-light/00ff00/0.2"
      0
      (compare-image 'hard-light "00ff00" "0.2"))
    (test
      "10.03.02: hard-light/00ff00/0.43"
      0
      (compare-image 'hard-light "00ff00" "0.43"))
    (test
      "10.03.03: hard-light/00ff00/0.6"
      0
      (compare-image 'hard-light "00ff00" "0.6"))
    (test
      "10.03.04: hard-light/00ff00/0.78"
      0
      (compare-image 'hard-light "00ff00" "0.78"))
    (test
      "10.03.05: hard-light/00ff00/1.0"
      0
      (compare-image 'hard-light "00ff00" "1.0"))
  )
  (test-group "[10.04] hard-light/27249c"
    (test
      "10.04.01: hard-light/27249c/0.2"
      0
      (compare-image 'hard-light "27249c" "0.2"))
    (test
      "10.04.02: hard-light/27249c/0.43"
      0
      (compare-image 'hard-light "27249c" "0.43"))
    (test
      "10.04.03: hard-light/27249c/0.6"
      0
      (compare-image 'hard-light "27249c" "0.6"))
    (test
      "10.04.04: hard-light/27249c/0.78"
      0
      (compare-image 'hard-light "27249c" "0.78"))
    (test
      "10.04.05: hard-light/27249c/1.0"
      0
      (compare-image 'hard-light "27249c" "1.0"))
  )
  (test-group "[10.05] hard-light/54fa0d"
    (test
      "10.05.01: hard-light/54fa0d/0.2"
      0
      (compare-image 'hard-light "54fa0d" "0.2"))
    (test
      "10.05.02: hard-light/54fa0d/0.43"
      0
      (compare-image 'hard-light "54fa0d" "0.43"))
    (test
      "10.05.03: hard-light/54fa0d/0.6"
      0
      (compare-image 'hard-light "54fa0d" "0.6"))
    (test
      "10.05.04: hard-light/54fa0d/0.78"
      0
      (compare-image 'hard-light "54fa0d" "0.78"))
    (test
      "10.05.05: hard-light/54fa0d/1.0"
      0
      (compare-image 'hard-light "54fa0d" "1.0"))
  )
  (test-group "[10.06] hard-light/5c5c5c"
    (test
      "10.06.01: hard-light/5c5c5c/0.2"
      0
      (compare-image 'hard-light "5c5c5c" "0.2"))
    (test
      "10.06.02: hard-light/5c5c5c/0.43"
      0
      (compare-image 'hard-light "5c5c5c" "0.43"))
    (test
      "10.06.03: hard-light/5c5c5c/0.6"
      0
      (compare-image 'hard-light "5c5c5c" "0.6"))
    (test
      "10.06.04: hard-light/5c5c5c/0.78"
      0
      (compare-image 'hard-light "5c5c5c" "0.78"))
    (test
      "10.06.05: hard-light/5c5c5c/1.0"
      0
      (compare-image 'hard-light "5c5c5c" "1.0"))
  )
  (test-group "[10.07] hard-light/775acf"
    (test
      "10.07.01: hard-light/775acf/0.2"
      0
      (compare-image 'hard-light "775acf" "0.2"))
    (test
      "10.07.02: hard-light/775acf/0.43"
      0
      (compare-image 'hard-light "775acf" "0.43"))
    (test
      "10.07.03: hard-light/775acf/0.6"
      0
      (compare-image 'hard-light "775acf" "0.6"))
    (test
      "10.07.04: hard-light/775acf/0.78"
      0
      (compare-image 'hard-light "775acf" "0.78"))
    (test
      "10.07.05: hard-light/775acf/1.0"
      0
      (compare-image 'hard-light "775acf" "1.0"))
  )
  (test-group "[10.08] hard-light/7f7d0a"
    (test
      "10.08.01: hard-light/7f7d0a/0.2"
      0
      (compare-image 'hard-light "7f7d0a" "0.2"))
    (test
      "10.08.02: hard-light/7f7d0a/0.43"
      0
      (compare-image 'hard-light "7f7d0a" "0.43"))
    (test
      "10.08.03: hard-light/7f7d0a/0.6"
      0
      (compare-image 'hard-light "7f7d0a" "0.6"))
    (test
      "10.08.04: hard-light/7f7d0a/0.78"
      0
      (compare-image 'hard-light "7f7d0a" "0.78"))
    (test
      "10.08.05: hard-light/7f7d0a/1.0"
      0
      (compare-image 'hard-light "7f7d0a" "1.0"))
  )
  (test-group "[10.09] hard-light/808080"
    (test
      "10.09.01: hard-light/808080/0.2"
      0
      (compare-image 'hard-light "808080" "0.2"))
    (test
      "10.09.02: hard-light/808080/0.43"
      0
      (compare-image 'hard-light "808080" "0.43"))
    (test
      "10.09.03: hard-light/808080/0.6"
      0
      (compare-image 'hard-light "808080" "0.6"))
    (test
      "10.09.04: hard-light/808080/0.78"
      0
      (compare-image 'hard-light "808080" "0.78"))
    (test
      "10.09.05: hard-light/808080/1.0"
      0
      (compare-image 'hard-light "808080" "1.0"))
  )
  (test-group "[10.10] hard-light/899675"
    (test
      "10.10.01: hard-light/899675/0.2"
      0
      (compare-image 'hard-light "899675" "0.2"))
    (test
      "10.10.02: hard-light/899675/0.43"
      0
      (compare-image 'hard-light "899675" "0.43"))
    (test
      "10.10.03: hard-light/899675/0.6"
      0
      (compare-image 'hard-light "899675" "0.6"))
    (test
      "10.10.04: hard-light/899675/0.78"
      0
      (compare-image 'hard-light "899675" "0.78"))
    (test
      "10.10.05: hard-light/899675/1.0"
      0
      (compare-image 'hard-light "899675" "1.0"))
  )
  (test-group "[10.11] hard-light/98d5e4"
    (test
      "10.11.01: hard-light/98d5e4/0.2"
      0
      (compare-image 'hard-light "98d5e4" "0.2"))
    (test
      "10.11.02: hard-light/98d5e4/0.43"
      0
      (compare-image 'hard-light "98d5e4" "0.43"))
    (test
      "10.11.03: hard-light/98d5e4/0.6"
      0
      (compare-image 'hard-light "98d5e4" "0.6"))
    (test
      "10.11.04: hard-light/98d5e4/0.78"
      0
      (compare-image 'hard-light "98d5e4" "0.78"))
    (test
      "10.11.05: hard-light/98d5e4/1.0"
      0
      (compare-image 'hard-light "98d5e4" "1.0"))
  )
  (test-group "[10.12] hard-light/b5b5b5"
    (test
      "10.12.01: hard-light/b5b5b5/0.2"
      0
      (compare-image 'hard-light "b5b5b5" "0.2"))
    (test
      "10.12.02: hard-light/b5b5b5/0.43"
      0
      (compare-image 'hard-light "b5b5b5" "0.43"))
    (test
      "10.12.03: hard-light/b5b5b5/0.6"
      0
      (compare-image 'hard-light "b5b5b5" "0.6"))
    (test
      "10.12.04: hard-light/b5b5b5/0.78"
      0
      (compare-image 'hard-light "b5b5b5" "0.78"))
    (test
      "10.12.05: hard-light/b5b5b5/1.0"
      0
      (compare-image 'hard-light "b5b5b5" "1.0"))
  )
  (test-group "[10.13] hard-light/cd1f3c"
    (test
      "10.13.01: hard-light/cd1f3c/0.2"
      0
      (compare-image 'hard-light "cd1f3c" "0.2"))
    (test
      "10.13.02: hard-light/cd1f3c/0.43"
      0
      (compare-image 'hard-light "cd1f3c" "0.43"))
    (test
      "10.13.03: hard-light/cd1f3c/0.6"
      0
      (compare-image 'hard-light "cd1f3c" "0.6"))
    (test
      "10.13.04: hard-light/cd1f3c/0.78"
      0
      (compare-image 'hard-light "cd1f3c" "0.78"))
    (test
      "10.13.05: hard-light/cd1f3c/1.0"
      0
      (compare-image 'hard-light "cd1f3c" "1.0"))
  )
  (test-group "[10.14] hard-light/f62db6"
    (test
      "10.14.01: hard-light/f62db6/0.2"
      0
      (compare-image 'hard-light "f62db6" "0.2"))
    (test
      "10.14.02: hard-light/f62db6/0.43"
      0
      (compare-image 'hard-light "f62db6" "0.43"))
    (test
      "10.14.03: hard-light/f62db6/0.6"
      0
      (compare-image 'hard-light "f62db6" "0.6"))
    (test
      "10.14.04: hard-light/f62db6/0.78"
      0
      (compare-image 'hard-light "f62db6" "0.78"))
    (test
      "10.14.05: hard-light/f62db6/1.0"
      0
      (compare-image 'hard-light "f62db6" "1.0"))
  )
  (test-group "[10.15] hard-light/ff0000"
    (test
      "10.15.01: hard-light/ff0000/0.2"
      0
      (compare-image 'hard-light "ff0000" "0.2"))
    (test
      "10.15.02: hard-light/ff0000/0.43"
      0
      (compare-image 'hard-light "ff0000" "0.43"))
    (test
      "10.15.03: hard-light/ff0000/0.6"
      0
      (compare-image 'hard-light "ff0000" "0.6"))
    (test
      "10.15.04: hard-light/ff0000/0.78"
      0
      (compare-image 'hard-light "ff0000" "0.78"))
    (test
      "10.15.05: hard-light/ff0000/1.0"
      0
      (compare-image 'hard-light "ff0000" "1.0"))
  )
  (test-group "[10.16] hard-light/ffffff"
    (test
      "10.16.01: hard-light/ffffff/0.2"
      0
      (compare-image 'hard-light "ffffff" "0.2"))
    (test
      "10.16.02: hard-light/ffffff/0.43"
      0
      (compare-image 'hard-light "ffffff" "0.43"))
    (test
      "10.16.03: hard-light/ffffff/0.6"
      0
      (compare-image 'hard-light "ffffff" "0.6"))
    (test
      "10.16.04: hard-light/ffffff/0.78"
      0
      (compare-image 'hard-light "ffffff" "0.78"))
    (test
      "10.16.05: hard-light/ffffff/1.0"
      0
      (compare-image 'hard-light "ffffff" "1.0"))
  )
)
(test-group "[11] hue"
  (test-group "[11.01] hue/000000"
    (test
      "11.01.01: hue/000000/0.2"
      0
      (compare-image 'hue "000000" "0.2"))
    (test
      "11.01.02: hue/000000/0.43"
      0
      (compare-image 'hue "000000" "0.43"))
    (test
      "11.01.03: hue/000000/0.6"
      0
      (compare-image 'hue "000000" "0.6"))
    (test
      "11.01.04: hue/000000/0.78"
      0
      (compare-image 'hue "000000" "0.78"))
    (test
      "11.01.05: hue/000000/1.0"
      0
      (compare-image 'hue "000000" "1.0"))
  )
  (test-group "[11.02] hue/0000ff"
    (test
      "11.02.01: hue/0000ff/0.2"
      0
      (compare-image 'hue "0000ff" "0.2"))
    (test
      "11.02.02: hue/0000ff/0.43"
      0
      (compare-image 'hue "0000ff" "0.43"))
    (test
      "11.02.03: hue/0000ff/0.6"
      0
      (compare-image 'hue "0000ff" "0.6"))
    (test
      "11.02.04: hue/0000ff/0.78"
      0
      (compare-image 'hue "0000ff" "0.78"))
    (test
      "11.02.05: hue/0000ff/1.0"
      0
      (compare-image 'hue "0000ff" "1.0"))
  )
  (test-group "[11.03] hue/00ff00"
    (test
      "11.03.01: hue/00ff00/0.2"
      0
      (compare-image 'hue "00ff00" "0.2"))
    (test
      "11.03.02: hue/00ff00/0.43"
      0
      (compare-image 'hue "00ff00" "0.43"))
    (test
      "11.03.03: hue/00ff00/0.6"
      0
      (compare-image 'hue "00ff00" "0.6"))
    (test
      "11.03.04: hue/00ff00/0.78"
      0
      (compare-image 'hue "00ff00" "0.78"))
    (test
      "11.03.05: hue/00ff00/1.0"
      0
      (compare-image 'hue "00ff00" "1.0"))
  )
  (test-group "[11.04] hue/27249c"
    (test
      "11.04.01: hue/27249c/0.2"
      0
      (compare-image 'hue "27249c" "0.2"))
    (test
      "11.04.02: hue/27249c/0.43"
      0
      (compare-image 'hue "27249c" "0.43"))
    (test
      "11.04.03: hue/27249c/0.6"
      0
      (compare-image 'hue "27249c" "0.6"))
    (test
      "11.04.04: hue/27249c/0.78"
      0
      (compare-image 'hue "27249c" "0.78"))
    (test
      "11.04.05: hue/27249c/1.0"
      0
      (compare-image 'hue "27249c" "1.0"))
  )
  (test-group "[11.05] hue/54fa0d"
    (test
      "11.05.01: hue/54fa0d/0.2"
      0
      (compare-image 'hue "54fa0d" "0.2"))
    (test
      "11.05.02: hue/54fa0d/0.43"
      0
      (compare-image 'hue "54fa0d" "0.43"))
    (test
      "11.05.03: hue/54fa0d/0.6"
      0
      (compare-image 'hue "54fa0d" "0.6"))
    (test
      "11.05.04: hue/54fa0d/0.78"
      0
      (compare-image 'hue "54fa0d" "0.78"))
    (test
      "11.05.05: hue/54fa0d/1.0"
      0
      (compare-image 'hue "54fa0d" "1.0"))
  )
  (test-group "[11.06] hue/5c5c5c"
    (test
      "11.06.01: hue/5c5c5c/0.2"
      0
      (compare-image 'hue "5c5c5c" "0.2"))
    (test
      "11.06.02: hue/5c5c5c/0.43"
      0
      (compare-image 'hue "5c5c5c" "0.43"))
    (test
      "11.06.03: hue/5c5c5c/0.6"
      0
      (compare-image 'hue "5c5c5c" "0.6"))
    (test
      "11.06.04: hue/5c5c5c/0.78"
      0
      (compare-image 'hue "5c5c5c" "0.78"))
    (test
      "11.06.05: hue/5c5c5c/1.0"
      0
      (compare-image 'hue "5c5c5c" "1.0"))
  )
  (test-group "[11.07] hue/775acf"
    (test
      "11.07.01: hue/775acf/0.2"
      0
      (compare-image 'hue "775acf" "0.2"))
    (test
      "11.07.02: hue/775acf/0.43"
      0
      (compare-image 'hue "775acf" "0.43"))
    (test
      "11.07.03: hue/775acf/0.6"
      0
      (compare-image 'hue "775acf" "0.6"))
    (test
      "11.07.04: hue/775acf/0.78"
      0
      (compare-image 'hue "775acf" "0.78"))
    (test
      "11.07.05: hue/775acf/1.0"
      0
      (compare-image 'hue "775acf" "1.0"))
  )
  (test-group "[11.08] hue/7f7d0a"
    (test
      "11.08.01: hue/7f7d0a/0.2"
      0
      (compare-image 'hue "7f7d0a" "0.2"))
    (test
      "11.08.02: hue/7f7d0a/0.43"
      0
      (compare-image 'hue "7f7d0a" "0.43"))
    (test
      "11.08.03: hue/7f7d0a/0.6"
      0
      (compare-image 'hue "7f7d0a" "0.6"))
    (test
      "11.08.04: hue/7f7d0a/0.78"
      0
      (compare-image 'hue "7f7d0a" "0.78"))
    (test
      "11.08.05: hue/7f7d0a/1.0"
      0
      (compare-image 'hue "7f7d0a" "1.0"))
  )
  (test-group "[11.09] hue/808080"
    (test
      "11.09.01: hue/808080/0.2"
      0
      (compare-image 'hue "808080" "0.2"))
    (test
      "11.09.02: hue/808080/0.43"
      0
      (compare-image 'hue "808080" "0.43"))
    (test
      "11.09.03: hue/808080/0.6"
      0
      (compare-image 'hue "808080" "0.6"))
    (test
      "11.09.04: hue/808080/0.78"
      0
      (compare-image 'hue "808080" "0.78"))
    (test
      "11.09.05: hue/808080/1.0"
      0
      (compare-image 'hue "808080" "1.0"))
  )
  (test-group "[11.10] hue/899675"
    (test
      "11.10.01: hue/899675/0.2"
      0
      (compare-image 'hue "899675" "0.2"))
    (test
      "11.10.02: hue/899675/0.43"
      0
      (compare-image 'hue "899675" "0.43"))
    (test
      "11.10.03: hue/899675/0.6"
      0
      (compare-image 'hue "899675" "0.6"))
    (test
      "11.10.04: hue/899675/0.78"
      0
      (compare-image 'hue "899675" "0.78"))
    (test
      "11.10.05: hue/899675/1.0"
      0
      (compare-image 'hue "899675" "1.0"))
  )
  (test-group "[11.11] hue/98d5e4"
    (test
      "11.11.01: hue/98d5e4/0.2"
      0
      (compare-image 'hue "98d5e4" "0.2"))
    (test
      "11.11.02: hue/98d5e4/0.43"
      0
      (compare-image 'hue "98d5e4" "0.43"))
    (test
      "11.11.03: hue/98d5e4/0.6"
      0
      (compare-image 'hue "98d5e4" "0.6"))
    (test
      "11.11.04: hue/98d5e4/0.78"
      0
      (compare-image 'hue "98d5e4" "0.78"))
    (test
      "11.11.05: hue/98d5e4/1.0"
      0
      (compare-image 'hue "98d5e4" "1.0"))
  )
  (test-group "[11.12] hue/b5b5b5"
    (test
      "11.12.01: hue/b5b5b5/0.2"
      0
      (compare-image 'hue "b5b5b5" "0.2"))
    (test
      "11.12.02: hue/b5b5b5/0.43"
      0
      (compare-image 'hue "b5b5b5" "0.43"))
    (test
      "11.12.03: hue/b5b5b5/0.6"
      0
      (compare-image 'hue "b5b5b5" "0.6"))
    (test
      "11.12.04: hue/b5b5b5/0.78"
      0
      (compare-image 'hue "b5b5b5" "0.78"))
    (test
      "11.12.05: hue/b5b5b5/1.0"
      0
      (compare-image 'hue "b5b5b5" "1.0"))
  )
  (test-group "[11.13] hue/cd1f3c"
    (test
      "11.13.01: hue/cd1f3c/0.2"
      0
      (compare-image 'hue "cd1f3c" "0.2"))
    (test
      "11.13.02: hue/cd1f3c/0.43"
      0
      (compare-image 'hue "cd1f3c" "0.43"))
    (test
      "11.13.03: hue/cd1f3c/0.6"
      0
      (compare-image 'hue "cd1f3c" "0.6"))
    (test
      "11.13.04: hue/cd1f3c/0.78"
      0
      (compare-image 'hue "cd1f3c" "0.78"))
    (test
      "11.13.05: hue/cd1f3c/1.0"
      0
      (compare-image 'hue "cd1f3c" "1.0"))
  )
  (test-group "[11.14] hue/f62db6"
    (test
      "11.14.01: hue/f62db6/0.2"
      0
      (compare-image 'hue "f62db6" "0.2"))
    (test
      "11.14.02: hue/f62db6/0.43"
      0
      (compare-image 'hue "f62db6" "0.43"))
    (test
      "11.14.03: hue/f62db6/0.6"
      0
      (compare-image 'hue "f62db6" "0.6"))
    (test
      "11.14.04: hue/f62db6/0.78"
      0
      (compare-image 'hue "f62db6" "0.78"))
    (test
      "11.14.05: hue/f62db6/1.0"
      0
      (compare-image 'hue "f62db6" "1.0"))
  )
  (test-group "[11.15] hue/ff0000"
    (test
      "11.15.01: hue/ff0000/0.2"
      0
      (compare-image 'hue "ff0000" "0.2"))
    (test
      "11.15.02: hue/ff0000/0.43"
      0
      (compare-image 'hue "ff0000" "0.43"))
    (test
      "11.15.03: hue/ff0000/0.6"
      0
      (compare-image 'hue "ff0000" "0.6"))
    (test
      "11.15.04: hue/ff0000/0.78"
      0
      (compare-image 'hue "ff0000" "0.78"))
    (test
      "11.15.05: hue/ff0000/1.0"
      0
      (compare-image 'hue "ff0000" "1.0"))
  )
  (test-group "[11.16] hue/ffffff"
    (test
      "11.16.01: hue/ffffff/0.2"
      0
      (compare-image 'hue "ffffff" "0.2"))
    (test
      "11.16.02: hue/ffffff/0.43"
      0
      (compare-image 'hue "ffffff" "0.43"))
    (test
      "11.16.03: hue/ffffff/0.6"
      0
      (compare-image 'hue "ffffff" "0.6"))
    (test
      "11.16.04: hue/ffffff/0.78"
      0
      (compare-image 'hue "ffffff" "0.78"))
    (test
      "11.16.05: hue/ffffff/1.0"
      0
      (compare-image 'hue "ffffff" "1.0"))
  )
)
(test-group "[12] lighten-only"
  (test-group "[12.01] lighten-only/000000"
    (test
      "12.01.01: lighten-only/000000/0.2"
      0
      (compare-image 'lighten-only "000000" "0.2"))
    (test
      "12.01.02: lighten-only/000000/0.43"
      0
      (compare-image 'lighten-only "000000" "0.43"))
    (test
      "12.01.03: lighten-only/000000/0.6"
      0
      (compare-image 'lighten-only "000000" "0.6"))
    (test
      "12.01.04: lighten-only/000000/0.78"
      0
      (compare-image 'lighten-only "000000" "0.78"))
    (test
      "12.01.05: lighten-only/000000/1.0"
      0
      (compare-image 'lighten-only "000000" "1.0"))
  )
  (test-group "[12.02] lighten-only/0000ff"
    (test
      "12.02.01: lighten-only/0000ff/0.2"
      0
      (compare-image 'lighten-only "0000ff" "0.2"))
    (test
      "12.02.02: lighten-only/0000ff/0.43"
      0
      (compare-image 'lighten-only "0000ff" "0.43"))
    (test
      "12.02.03: lighten-only/0000ff/0.6"
      0
      (compare-image 'lighten-only "0000ff" "0.6"))
    (test
      "12.02.04: lighten-only/0000ff/0.78"
      0
      (compare-image 'lighten-only "0000ff" "0.78"))
    (test
      "12.02.05: lighten-only/0000ff/1.0"
      0
      (compare-image 'lighten-only "0000ff" "1.0"))
  )
  (test-group "[12.03] lighten-only/00ff00"
    (test
      "12.03.01: lighten-only/00ff00/0.2"
      0
      (compare-image 'lighten-only "00ff00" "0.2"))
    (test
      "12.03.02: lighten-only/00ff00/0.43"
      0
      (compare-image 'lighten-only "00ff00" "0.43"))
    (test
      "12.03.03: lighten-only/00ff00/0.6"
      0
      (compare-image 'lighten-only "00ff00" "0.6"))
    (test
      "12.03.04: lighten-only/00ff00/0.78"
      0
      (compare-image 'lighten-only "00ff00" "0.78"))
    (test
      "12.03.05: lighten-only/00ff00/1.0"
      0
      (compare-image 'lighten-only "00ff00" "1.0"))
  )
  (test-group "[12.04] lighten-only/27249c"
    (test
      "12.04.01: lighten-only/27249c/0.2"
      0
      (compare-image 'lighten-only "27249c" "0.2"))
    (test
      "12.04.02: lighten-only/27249c/0.43"
      0
      (compare-image 'lighten-only "27249c" "0.43"))
    (test
      "12.04.03: lighten-only/27249c/0.6"
      0
      (compare-image 'lighten-only "27249c" "0.6"))
    (test
      "12.04.04: lighten-only/27249c/0.78"
      0
      (compare-image 'lighten-only "27249c" "0.78"))
    (test
      "12.04.05: lighten-only/27249c/1.0"
      0
      (compare-image 'lighten-only "27249c" "1.0"))
  )
  (test-group "[12.05] lighten-only/54fa0d"
    (test
      "12.05.01: lighten-only/54fa0d/0.2"
      0
      (compare-image 'lighten-only "54fa0d" "0.2"))
    (test
      "12.05.02: lighten-only/54fa0d/0.43"
      0
      (compare-image 'lighten-only "54fa0d" "0.43"))
    (test
      "12.05.03: lighten-only/54fa0d/0.6"
      0
      (compare-image 'lighten-only "54fa0d" "0.6"))
    (test
      "12.05.04: lighten-only/54fa0d/0.78"
      0
      (compare-image 'lighten-only "54fa0d" "0.78"))
    (test
      "12.05.05: lighten-only/54fa0d/1.0"
      0
      (compare-image 'lighten-only "54fa0d" "1.0"))
  )
  (test-group "[12.06] lighten-only/5c5c5c"
    (test
      "12.06.01: lighten-only/5c5c5c/0.2"
      0
      (compare-image 'lighten-only "5c5c5c" "0.2"))
    (test
      "12.06.02: lighten-only/5c5c5c/0.43"
      0
      (compare-image 'lighten-only "5c5c5c" "0.43"))
    (test
      "12.06.03: lighten-only/5c5c5c/0.6"
      0
      (compare-image 'lighten-only "5c5c5c" "0.6"))
    (test
      "12.06.04: lighten-only/5c5c5c/0.78"
      0
      (compare-image 'lighten-only "5c5c5c" "0.78"))
    (test
      "12.06.05: lighten-only/5c5c5c/1.0"
      0
      (compare-image 'lighten-only "5c5c5c" "1.0"))
  )
  (test-group "[12.07] lighten-only/775acf"
    (test
      "12.07.01: lighten-only/775acf/0.2"
      0
      (compare-image 'lighten-only "775acf" "0.2"))
    (test
      "12.07.02: lighten-only/775acf/0.43"
      0
      (compare-image 'lighten-only "775acf" "0.43"))
    (test
      "12.07.03: lighten-only/775acf/0.6"
      0
      (compare-image 'lighten-only "775acf" "0.6"))
    (test
      "12.07.04: lighten-only/775acf/0.78"
      0
      (compare-image 'lighten-only "775acf" "0.78"))
    (test
      "12.07.05: lighten-only/775acf/1.0"
      0
      (compare-image 'lighten-only "775acf" "1.0"))
  )
  (test-group "[12.08] lighten-only/7f7d0a"
    (test
      "12.08.01: lighten-only/7f7d0a/0.2"
      0
      (compare-image 'lighten-only "7f7d0a" "0.2"))
    (test
      "12.08.02: lighten-only/7f7d0a/0.43"
      0
      (compare-image 'lighten-only "7f7d0a" "0.43"))
    (test
      "12.08.03: lighten-only/7f7d0a/0.6"
      0
      (compare-image 'lighten-only "7f7d0a" "0.6"))
    (test
      "12.08.04: lighten-only/7f7d0a/0.78"
      0
      (compare-image 'lighten-only "7f7d0a" "0.78"))
    (test
      "12.08.05: lighten-only/7f7d0a/1.0"
      0
      (compare-image 'lighten-only "7f7d0a" "1.0"))
  )
  (test-group "[12.09] lighten-only/808080"
    (test
      "12.09.01: lighten-only/808080/0.2"
      0
      (compare-image 'lighten-only "808080" "0.2"))
    (test
      "12.09.02: lighten-only/808080/0.43"
      0
      (compare-image 'lighten-only "808080" "0.43"))
    (test
      "12.09.03: lighten-only/808080/0.6"
      0
      (compare-image 'lighten-only "808080" "0.6"))
    (test
      "12.09.04: lighten-only/808080/0.78"
      0
      (compare-image 'lighten-only "808080" "0.78"))
    (test
      "12.09.05: lighten-only/808080/1.0"
      0
      (compare-image 'lighten-only "808080" "1.0"))
  )
  (test-group "[12.10] lighten-only/899675"
    (test
      "12.10.01: lighten-only/899675/0.2"
      0
      (compare-image 'lighten-only "899675" "0.2"))
    (test
      "12.10.02: lighten-only/899675/0.43"
      0
      (compare-image 'lighten-only "899675" "0.43"))
    (test
      "12.10.03: lighten-only/899675/0.6"
      0
      (compare-image 'lighten-only "899675" "0.6"))
    (test
      "12.10.04: lighten-only/899675/0.78"
      0
      (compare-image 'lighten-only "899675" "0.78"))
    (test
      "12.10.05: lighten-only/899675/1.0"
      0
      (compare-image 'lighten-only "899675" "1.0"))
  )
  (test-group "[12.11] lighten-only/98d5e4"
    (test
      "12.11.01: lighten-only/98d5e4/0.2"
      0
      (compare-image 'lighten-only "98d5e4" "0.2"))
    (test
      "12.11.02: lighten-only/98d5e4/0.43"
      0
      (compare-image 'lighten-only "98d5e4" "0.43"))
    (test
      "12.11.03: lighten-only/98d5e4/0.6"
      0
      (compare-image 'lighten-only "98d5e4" "0.6"))
    (test
      "12.11.04: lighten-only/98d5e4/0.78"
      0
      (compare-image 'lighten-only "98d5e4" "0.78"))
    (test
      "12.11.05: lighten-only/98d5e4/1.0"
      0
      (compare-image 'lighten-only "98d5e4" "1.0"))
  )
  (test-group "[12.12] lighten-only/b5b5b5"
    (test
      "12.12.01: lighten-only/b5b5b5/0.2"
      0
      (compare-image 'lighten-only "b5b5b5" "0.2"))
    (test
      "12.12.02: lighten-only/b5b5b5/0.43"
      0
      (compare-image 'lighten-only "b5b5b5" "0.43"))
    (test
      "12.12.03: lighten-only/b5b5b5/0.6"
      0
      (compare-image 'lighten-only "b5b5b5" "0.6"))
    (test
      "12.12.04: lighten-only/b5b5b5/0.78"
      0
      (compare-image 'lighten-only "b5b5b5" "0.78"))
    (test
      "12.12.05: lighten-only/b5b5b5/1.0"
      0
      (compare-image 'lighten-only "b5b5b5" "1.0"))
  )
  (test-group "[12.13] lighten-only/cd1f3c"
    (test
      "12.13.01: lighten-only/cd1f3c/0.2"
      0
      (compare-image 'lighten-only "cd1f3c" "0.2"))
    (test
      "12.13.02: lighten-only/cd1f3c/0.43"
      0
      (compare-image 'lighten-only "cd1f3c" "0.43"))
    (test
      "12.13.03: lighten-only/cd1f3c/0.6"
      0
      (compare-image 'lighten-only "cd1f3c" "0.6"))
    (test
      "12.13.04: lighten-only/cd1f3c/0.78"
      0
      (compare-image 'lighten-only "cd1f3c" "0.78"))
    (test
      "12.13.05: lighten-only/cd1f3c/1.0"
      0
      (compare-image 'lighten-only "cd1f3c" "1.0"))
  )
  (test-group "[12.14] lighten-only/f62db6"
    (test
      "12.14.01: lighten-only/f62db6/0.2"
      0
      (compare-image 'lighten-only "f62db6" "0.2"))
    (test
      "12.14.02: lighten-only/f62db6/0.43"
      0
      (compare-image 'lighten-only "f62db6" "0.43"))
    (test
      "12.14.03: lighten-only/f62db6/0.6"
      0
      (compare-image 'lighten-only "f62db6" "0.6"))
    (test
      "12.14.04: lighten-only/f62db6/0.78"
      0
      (compare-image 'lighten-only "f62db6" "0.78"))
    (test
      "12.14.05: lighten-only/f62db6/1.0"
      0
      (compare-image 'lighten-only "f62db6" "1.0"))
  )
  (test-group "[12.15] lighten-only/ff0000"
    (test
      "12.15.01: lighten-only/ff0000/0.2"
      0
      (compare-image 'lighten-only "ff0000" "0.2"))
    (test
      "12.15.02: lighten-only/ff0000/0.43"
      0
      (compare-image 'lighten-only "ff0000" "0.43"))
    (test
      "12.15.03: lighten-only/ff0000/0.6"
      0
      (compare-image 'lighten-only "ff0000" "0.6"))
    (test
      "12.15.04: lighten-only/ff0000/0.78"
      0
      (compare-image 'lighten-only "ff0000" "0.78"))
    (test
      "12.15.05: lighten-only/ff0000/1.0"
      0
      (compare-image 'lighten-only "ff0000" "1.0"))
  )
  (test-group "[12.16] lighten-only/ffffff"
    (test
      "12.16.01: lighten-only/ffffff/0.2"
      0
      (compare-image 'lighten-only "ffffff" "0.2"))
    (test
      "12.16.02: lighten-only/ffffff/0.43"
      0
      (compare-image 'lighten-only "ffffff" "0.43"))
    (test
      "12.16.03: lighten-only/ffffff/0.6"
      0
      (compare-image 'lighten-only "ffffff" "0.6"))
    (test
      "12.16.04: lighten-only/ffffff/0.78"
      0
      (compare-image 'lighten-only "ffffff" "0.78"))
    (test
      "12.16.05: lighten-only/ffffff/1.0"
      0
      (compare-image 'lighten-only "ffffff" "1.0"))
  )
)
(test-group "[13] multiply"
  (test-group "[13.01] multiply/000000"
    (test
      "13.01.01: multiply/000000/0.2"
      0
      (compare-image 'multiply "000000" "0.2"))
    (test
      "13.01.02: multiply/000000/0.43"
      0
      (compare-image 'multiply "000000" "0.43"))
    (test
      "13.01.03: multiply/000000/0.6"
      0
      (compare-image 'multiply "000000" "0.6"))
    (test
      "13.01.04: multiply/000000/0.78"
      0
      (compare-image 'multiply "000000" "0.78"))
    (test
      "13.01.05: multiply/000000/1.0"
      0
      (compare-image 'multiply "000000" "1.0"))
  )
  (test-group "[13.02] multiply/0000ff"
    (test
      "13.02.01: multiply/0000ff/0.2"
      0
      (compare-image 'multiply "0000ff" "0.2"))
    (test
      "13.02.02: multiply/0000ff/0.43"
      0
      (compare-image 'multiply "0000ff" "0.43"))
    (test
      "13.02.03: multiply/0000ff/0.6"
      0
      (compare-image 'multiply "0000ff" "0.6"))
    (test
      "13.02.04: multiply/0000ff/0.78"
      0
      (compare-image 'multiply "0000ff" "0.78"))
    (test
      "13.02.05: multiply/0000ff/1.0"
      0
      (compare-image 'multiply "0000ff" "1.0"))
  )
  (test-group "[13.03] multiply/00ff00"
    (test
      "13.03.01: multiply/00ff00/0.2"
      0
      (compare-image 'multiply "00ff00" "0.2"))
    (test
      "13.03.02: multiply/00ff00/0.43"
      0
      (compare-image 'multiply "00ff00" "0.43"))
    (test
      "13.03.03: multiply/00ff00/0.6"
      0
      (compare-image 'multiply "00ff00" "0.6"))
    (test
      "13.03.04: multiply/00ff00/0.78"
      0
      (compare-image 'multiply "00ff00" "0.78"))
    (test
      "13.03.05: multiply/00ff00/1.0"
      0
      (compare-image 'multiply "00ff00" "1.0"))
  )
  (test-group "[13.04] multiply/27249c"
    (test
      "13.04.01: multiply/27249c/0.2"
      0
      (compare-image 'multiply "27249c" "0.2"))
    (test
      "13.04.02: multiply/27249c/0.43"
      0
      (compare-image 'multiply "27249c" "0.43"))
    (test
      "13.04.03: multiply/27249c/0.6"
      0
      (compare-image 'multiply "27249c" "0.6"))
    (test
      "13.04.04: multiply/27249c/0.78"
      0
      (compare-image 'multiply "27249c" "0.78"))
    (test
      "13.04.05: multiply/27249c/1.0"
      0
      (compare-image 'multiply "27249c" "1.0"))
  )
  (test-group "[13.05] multiply/54fa0d"
    (test
      "13.05.01: multiply/54fa0d/0.2"
      0
      (compare-image 'multiply "54fa0d" "0.2"))
    (test
      "13.05.02: multiply/54fa0d/0.43"
      0
      (compare-image 'multiply "54fa0d" "0.43"))
    (test
      "13.05.03: multiply/54fa0d/0.6"
      0
      (compare-image 'multiply "54fa0d" "0.6"))
    (test
      "13.05.04: multiply/54fa0d/0.78"
      0
      (compare-image 'multiply "54fa0d" "0.78"))
    (test
      "13.05.05: multiply/54fa0d/1.0"
      0
      (compare-image 'multiply "54fa0d" "1.0"))
  )
  (test-group "[13.06] multiply/5c5c5c"
    (test
      "13.06.01: multiply/5c5c5c/0.2"
      0
      (compare-image 'multiply "5c5c5c" "0.2"))
    (test
      "13.06.02: multiply/5c5c5c/0.43"
      0
      (compare-image 'multiply "5c5c5c" "0.43"))
    (test
      "13.06.03: multiply/5c5c5c/0.6"
      0
      (compare-image 'multiply "5c5c5c" "0.6"))
    (test
      "13.06.04: multiply/5c5c5c/0.78"
      0
      (compare-image 'multiply "5c5c5c" "0.78"))
    (test
      "13.06.05: multiply/5c5c5c/1.0"
      0
      (compare-image 'multiply "5c5c5c" "1.0"))
  )
  (test-group "[13.07] multiply/775acf"
    (test
      "13.07.01: multiply/775acf/0.2"
      0
      (compare-image 'multiply "775acf" "0.2"))
    (test
      "13.07.02: multiply/775acf/0.43"
      0
      (compare-image 'multiply "775acf" "0.43"))
    (test
      "13.07.03: multiply/775acf/0.6"
      0
      (compare-image 'multiply "775acf" "0.6"))
    (test
      "13.07.04: multiply/775acf/0.78"
      0
      (compare-image 'multiply "775acf" "0.78"))
    (test
      "13.07.05: multiply/775acf/1.0"
      0
      (compare-image 'multiply "775acf" "1.0"))
  )
  (test-group "[13.08] multiply/7f7d0a"
    (test
      "13.08.01: multiply/7f7d0a/0.2"
      0
      (compare-image 'multiply "7f7d0a" "0.2"))
    (test
      "13.08.02: multiply/7f7d0a/0.43"
      0
      (compare-image 'multiply "7f7d0a" "0.43"))
    (test
      "13.08.03: multiply/7f7d0a/0.6"
      0
      (compare-image 'multiply "7f7d0a" "0.6"))
    (test
      "13.08.04: multiply/7f7d0a/0.78"
      0
      (compare-image 'multiply "7f7d0a" "0.78"))
    (test
      "13.08.05: multiply/7f7d0a/1.0"
      0
      (compare-image 'multiply "7f7d0a" "1.0"))
  )
  (test-group "[13.09] multiply/808080"
    (test
      "13.09.01: multiply/808080/0.2"
      0
      (compare-image 'multiply "808080" "0.2"))
    (test
      "13.09.02: multiply/808080/0.43"
      0
      (compare-image 'multiply "808080" "0.43"))
    (test
      "13.09.03: multiply/808080/0.6"
      0
      (compare-image 'multiply "808080" "0.6"))
    (test
      "13.09.04: multiply/808080/0.78"
      0
      (compare-image 'multiply "808080" "0.78"))
    (test
      "13.09.05: multiply/808080/1.0"
      0
      (compare-image 'multiply "808080" "1.0"))
  )
  (test-group "[13.10] multiply/899675"
    (test
      "13.10.01: multiply/899675/0.2"
      0
      (compare-image 'multiply "899675" "0.2"))
    (test
      "13.10.02: multiply/899675/0.43"
      0
      (compare-image 'multiply "899675" "0.43"))
    (test
      "13.10.03: multiply/899675/0.6"
      0
      (compare-image 'multiply "899675" "0.6"))
    (test
      "13.10.04: multiply/899675/0.78"
      0
      (compare-image 'multiply "899675" "0.78"))
    (test
      "13.10.05: multiply/899675/1.0"
      0
      (compare-image 'multiply "899675" "1.0"))
  )
  (test-group "[13.11] multiply/98d5e4"
    (test
      "13.11.01: multiply/98d5e4/0.2"
      0
      (compare-image 'multiply "98d5e4" "0.2"))
    (test
      "13.11.02: multiply/98d5e4/0.43"
      0
      (compare-image 'multiply "98d5e4" "0.43"))
    (test
      "13.11.03: multiply/98d5e4/0.6"
      0
      (compare-image 'multiply "98d5e4" "0.6"))
    (test
      "13.11.04: multiply/98d5e4/0.78"
      0
      (compare-image 'multiply "98d5e4" "0.78"))
    (test
      "13.11.05: multiply/98d5e4/1.0"
      0
      (compare-image 'multiply "98d5e4" "1.0"))
  )
  (test-group "[13.12] multiply/b5b5b5"
    (test
      "13.12.01: multiply/b5b5b5/0.2"
      0
      (compare-image 'multiply "b5b5b5" "0.2"))
    (test
      "13.12.02: multiply/b5b5b5/0.43"
      0
      (compare-image 'multiply "b5b5b5" "0.43"))
    (test
      "13.12.03: multiply/b5b5b5/0.6"
      0
      (compare-image 'multiply "b5b5b5" "0.6"))
    (test
      "13.12.04: multiply/b5b5b5/0.78"
      0
      (compare-image 'multiply "b5b5b5" "0.78"))
    (test
      "13.12.05: multiply/b5b5b5/1.0"
      0
      (compare-image 'multiply "b5b5b5" "1.0"))
  )
  (test-group "[13.13] multiply/cd1f3c"
    (test
      "13.13.01: multiply/cd1f3c/0.2"
      0
      (compare-image 'multiply "cd1f3c" "0.2"))
    (test
      "13.13.02: multiply/cd1f3c/0.43"
      0
      (compare-image 'multiply "cd1f3c" "0.43"))
    (test
      "13.13.03: multiply/cd1f3c/0.6"
      0
      (compare-image 'multiply "cd1f3c" "0.6"))
    (test
      "13.13.04: multiply/cd1f3c/0.78"
      0
      (compare-image 'multiply "cd1f3c" "0.78"))
    (test
      "13.13.05: multiply/cd1f3c/1.0"
      0
      (compare-image 'multiply "cd1f3c" "1.0"))
  )
  (test-group "[13.14] multiply/f62db6"
    (test
      "13.14.01: multiply/f62db6/0.2"
      0
      (compare-image 'multiply "f62db6" "0.2"))
    (test
      "13.14.02: multiply/f62db6/0.43"
      0
      (compare-image 'multiply "f62db6" "0.43"))
    (test
      "13.14.03: multiply/f62db6/0.6"
      0
      (compare-image 'multiply "f62db6" "0.6"))
    (test
      "13.14.04: multiply/f62db6/0.78"
      0
      (compare-image 'multiply "f62db6" "0.78"))
    (test
      "13.14.05: multiply/f62db6/1.0"
      0
      (compare-image 'multiply "f62db6" "1.0"))
  )
  (test-group "[13.15] multiply/ff0000"
    (test
      "13.15.01: multiply/ff0000/0.2"
      0
      (compare-image 'multiply "ff0000" "0.2"))
    (test
      "13.15.02: multiply/ff0000/0.43"
      0
      (compare-image 'multiply "ff0000" "0.43"))
    (test
      "13.15.03: multiply/ff0000/0.6"
      0
      (compare-image 'multiply "ff0000" "0.6"))
    (test
      "13.15.04: multiply/ff0000/0.78"
      0
      (compare-image 'multiply "ff0000" "0.78"))
    (test
      "13.15.05: multiply/ff0000/1.0"
      0
      (compare-image 'multiply "ff0000" "1.0"))
  )
  (test-group "[13.16] multiply/ffffff"
    (test
      "13.16.01: multiply/ffffff/0.2"
      0
      (compare-image 'multiply "ffffff" "0.2"))
    (test
      "13.16.02: multiply/ffffff/0.43"
      0
      (compare-image 'multiply "ffffff" "0.43"))
    (test
      "13.16.03: multiply/ffffff/0.6"
      0
      (compare-image 'multiply "ffffff" "0.6"))
    (test
      "13.16.04: multiply/ffffff/0.78"
      0
      (compare-image 'multiply "ffffff" "0.78"))
    (test
      "13.16.05: multiply/ffffff/1.0"
      0
      (compare-image 'multiply "ffffff" "1.0"))
  )
)
(test-group "[14] normal"
  (test-group "[14.01] normal/000000"
    (test
      "14.01.01: normal/000000/0.2"
      0
      (compare-image 'normal "000000" "0.2"))
    (test
      "14.01.02: normal/000000/0.43"
      0
      (compare-image 'normal "000000" "0.43"))
    (test
      "14.01.03: normal/000000/0.6"
      0
      (compare-image 'normal "000000" "0.6"))
    (test
      "14.01.04: normal/000000/0.78"
      0
      (compare-image 'normal "000000" "0.78"))
    (test
      "14.01.05: normal/000000/1.0"
      0
      (compare-image 'normal "000000" "1.0"))
  )
  (test-group "[14.02] normal/0000ff"
    (test
      "14.02.01: normal/0000ff/0.2"
      0
      (compare-image 'normal "0000ff" "0.2"))
    (test
      "14.02.02: normal/0000ff/0.43"
      0
      (compare-image 'normal "0000ff" "0.43"))
    (test
      "14.02.03: normal/0000ff/0.6"
      0
      (compare-image 'normal "0000ff" "0.6"))
    (test
      "14.02.04: normal/0000ff/0.78"
      0
      (compare-image 'normal "0000ff" "0.78"))
    (test
      "14.02.05: normal/0000ff/1.0"
      0
      (compare-image 'normal "0000ff" "1.0"))
  )
  (test-group "[14.03] normal/00ff00"
    (test
      "14.03.01: normal/00ff00/0.2"
      0
      (compare-image 'normal "00ff00" "0.2"))
    (test
      "14.03.02: normal/00ff00/0.43"
      0
      (compare-image 'normal "00ff00" "0.43"))
    (test
      "14.03.03: normal/00ff00/0.6"
      0
      (compare-image 'normal "00ff00" "0.6"))
    (test
      "14.03.04: normal/00ff00/0.78"
      0
      (compare-image 'normal "00ff00" "0.78"))
    (test
      "14.03.05: normal/00ff00/1.0"
      0
      (compare-image 'normal "00ff00" "1.0"))
  )
  (test-group "[14.04] normal/27249c"
    (test
      "14.04.01: normal/27249c/0.2"
      0
      (compare-image 'normal "27249c" "0.2"))
    (test
      "14.04.02: normal/27249c/0.43"
      0
      (compare-image 'normal "27249c" "0.43"))
    (test
      "14.04.03: normal/27249c/0.6"
      0
      (compare-image 'normal "27249c" "0.6"))
    (test
      "14.04.04: normal/27249c/0.78"
      0
      (compare-image 'normal "27249c" "0.78"))
    (test
      "14.04.05: normal/27249c/1.0"
      0
      (compare-image 'normal "27249c" "1.0"))
  )
  (test-group "[14.05] normal/54fa0d"
    (test
      "14.05.01: normal/54fa0d/0.2"
      0
      (compare-image 'normal "54fa0d" "0.2"))
    (test
      "14.05.02: normal/54fa0d/0.43"
      0
      (compare-image 'normal "54fa0d" "0.43"))
    (test
      "14.05.03: normal/54fa0d/0.6"
      0
      (compare-image 'normal "54fa0d" "0.6"))
    (test
      "14.05.04: normal/54fa0d/0.78"
      0
      (compare-image 'normal "54fa0d" "0.78"))
    (test
      "14.05.05: normal/54fa0d/1.0"
      0
      (compare-image 'normal "54fa0d" "1.0"))
  )
  (test-group "[14.06] normal/5c5c5c"
    (test
      "14.06.01: normal/5c5c5c/0.2"
      0
      (compare-image 'normal "5c5c5c" "0.2"))
    (test
      "14.06.02: normal/5c5c5c/0.43"
      0
      (compare-image 'normal "5c5c5c" "0.43"))
    (test
      "14.06.03: normal/5c5c5c/0.6"
      0
      (compare-image 'normal "5c5c5c" "0.6"))
    (test
      "14.06.04: normal/5c5c5c/0.78"
      0
      (compare-image 'normal "5c5c5c" "0.78"))
    (test
      "14.06.05: normal/5c5c5c/1.0"
      0
      (compare-image 'normal "5c5c5c" "1.0"))
  )
  (test-group "[14.07] normal/775acf"
    (test
      "14.07.01: normal/775acf/0.2"
      0
      (compare-image 'normal "775acf" "0.2"))
    (test
      "14.07.02: normal/775acf/0.43"
      0
      (compare-image 'normal "775acf" "0.43"))
    (test
      "14.07.03: normal/775acf/0.6"
      0
      (compare-image 'normal "775acf" "0.6"))
    (test
      "14.07.04: normal/775acf/0.78"
      0
      (compare-image 'normal "775acf" "0.78"))
    (test
      "14.07.05: normal/775acf/1.0"
      0
      (compare-image 'normal "775acf" "1.0"))
  )
  (test-group "[14.08] normal/7f7d0a"
    (test
      "14.08.01: normal/7f7d0a/0.2"
      0
      (compare-image 'normal "7f7d0a" "0.2"))
    (test
      "14.08.02: normal/7f7d0a/0.43"
      0
      (compare-image 'normal "7f7d0a" "0.43"))
    (test
      "14.08.03: normal/7f7d0a/0.6"
      0
      (compare-image 'normal "7f7d0a" "0.6"))
    (test
      "14.08.04: normal/7f7d0a/0.78"
      0
      (compare-image 'normal "7f7d0a" "0.78"))
    (test
      "14.08.05: normal/7f7d0a/1.0"
      0
      (compare-image 'normal "7f7d0a" "1.0"))
  )
  (test-group "[14.09] normal/808080"
    (test
      "14.09.01: normal/808080/0.2"
      0
      (compare-image 'normal "808080" "0.2"))
    (test
      "14.09.02: normal/808080/0.43"
      0
      (compare-image 'normal "808080" "0.43"))
    (test
      "14.09.03: normal/808080/0.6"
      0
      (compare-image 'normal "808080" "0.6"))
    (test
      "14.09.04: normal/808080/0.78"
      0
      (compare-image 'normal "808080" "0.78"))
    (test
      "14.09.05: normal/808080/1.0"
      0
      (compare-image 'normal "808080" "1.0"))
  )
  (test-group "[14.10] normal/899675"
    (test
      "14.10.01: normal/899675/0.2"
      0
      (compare-image 'normal "899675" "0.2"))
    (test
      "14.10.02: normal/899675/0.43"
      0
      (compare-image 'normal "899675" "0.43"))
    (test
      "14.10.03: normal/899675/0.6"
      0
      (compare-image 'normal "899675" "0.6"))
    (test
      "14.10.04: normal/899675/0.78"
      0
      (compare-image 'normal "899675" "0.78"))
    (test
      "14.10.05: normal/899675/1.0"
      0
      (compare-image 'normal "899675" "1.0"))
  )
  (test-group "[14.11] normal/98d5e4"
    (test
      "14.11.01: normal/98d5e4/0.2"
      0
      (compare-image 'normal "98d5e4" "0.2"))
    (test
      "14.11.02: normal/98d5e4/0.43"
      0
      (compare-image 'normal "98d5e4" "0.43"))
    (test
      "14.11.03: normal/98d5e4/0.6"
      0
      (compare-image 'normal "98d5e4" "0.6"))
    (test
      "14.11.04: normal/98d5e4/0.78"
      0
      (compare-image 'normal "98d5e4" "0.78"))
    (test
      "14.11.05: normal/98d5e4/1.0"
      0
      (compare-image 'normal "98d5e4" "1.0"))
  )
  (test-group "[14.12] normal/b5b5b5"
    (test
      "14.12.01: normal/b5b5b5/0.2"
      0
      (compare-image 'normal "b5b5b5" "0.2"))
    (test
      "14.12.02: normal/b5b5b5/0.43"
      0
      (compare-image 'normal "b5b5b5" "0.43"))
    (test
      "14.12.03: normal/b5b5b5/0.6"
      0
      (compare-image 'normal "b5b5b5" "0.6"))
    (test
      "14.12.04: normal/b5b5b5/0.78"
      0
      (compare-image 'normal "b5b5b5" "0.78"))
    (test
      "14.12.05: normal/b5b5b5/1.0"
      0
      (compare-image 'normal "b5b5b5" "1.0"))
  )
  (test-group "[14.13] normal/cd1f3c"
    (test
      "14.13.01: normal/cd1f3c/0.2"
      0
      (compare-image 'normal "cd1f3c" "0.2"))
    (test
      "14.13.02: normal/cd1f3c/0.43"
      0
      (compare-image 'normal "cd1f3c" "0.43"))
    (test
      "14.13.03: normal/cd1f3c/0.6"
      0
      (compare-image 'normal "cd1f3c" "0.6"))
    (test
      "14.13.04: normal/cd1f3c/0.78"
      0
      (compare-image 'normal "cd1f3c" "0.78"))
    (test
      "14.13.05: normal/cd1f3c/1.0"
      0
      (compare-image 'normal "cd1f3c" "1.0"))
  )
  (test-group "[14.14] normal/f62db6"
    (test
      "14.14.01: normal/f62db6/0.2"
      0
      (compare-image 'normal "f62db6" "0.2"))
    (test
      "14.14.02: normal/f62db6/0.43"
      0
      (compare-image 'normal "f62db6" "0.43"))
    (test
      "14.14.03: normal/f62db6/0.6"
      0
      (compare-image 'normal "f62db6" "0.6"))
    (test
      "14.14.04: normal/f62db6/0.78"
      0
      (compare-image 'normal "f62db6" "0.78"))
    (test
      "14.14.05: normal/f62db6/1.0"
      0
      (compare-image 'normal "f62db6" "1.0"))
  )
  (test-group "[14.15] normal/ff0000"
    (test
      "14.15.01: normal/ff0000/0.2"
      0
      (compare-image 'normal "ff0000" "0.2"))
    (test
      "14.15.02: normal/ff0000/0.43"
      0
      (compare-image 'normal "ff0000" "0.43"))
    (test
      "14.15.03: normal/ff0000/0.6"
      0
      (compare-image 'normal "ff0000" "0.6"))
    (test
      "14.15.04: normal/ff0000/0.78"
      0
      (compare-image 'normal "ff0000" "0.78"))
    (test
      "14.15.05: normal/ff0000/1.0"
      0
      (compare-image 'normal "ff0000" "1.0"))
  )
  (test-group "[14.16] normal/ffffff"
    (test
      "14.16.01: normal/ffffff/0.2"
      0
      (compare-image 'normal "ffffff" "0.2"))
    (test
      "14.16.02: normal/ffffff/0.43"
      0
      (compare-image 'normal "ffffff" "0.43"))
    (test
      "14.16.03: normal/ffffff/0.6"
      0
      (compare-image 'normal "ffffff" "0.6"))
    (test
      "14.16.04: normal/ffffff/0.78"
      0
      (compare-image 'normal "ffffff" "0.78"))
    (test
      "14.16.05: normal/ffffff/1.0"
      0
      (compare-image 'normal "ffffff" "1.0"))
  )
)
(test-group "[15] saturation"
  (test-group "[15.01] saturation/000000"
    (test
      "15.01.01: saturation/000000/0.2"
      0
      (compare-image 'saturation "000000" "0.2"))
    (test
      "15.01.02: saturation/000000/0.43"
      0
      (compare-image 'saturation "000000" "0.43"))
    (test
      "15.01.03: saturation/000000/0.6"
      0
      (compare-image 'saturation "000000" "0.6"))
    (test
      "15.01.04: saturation/000000/0.78"
      0
      (compare-image 'saturation "000000" "0.78"))
    (test
      "15.01.05: saturation/000000/1.0"
      0
      (compare-image 'saturation "000000" "1.0"))
  )
  (test-group "[15.02] saturation/0000ff"
    (test
      "15.02.01: saturation/0000ff/0.2"
      0
      (compare-image 'saturation "0000ff" "0.2"))
    (test
      "15.02.02: saturation/0000ff/0.43"
      0
      (compare-image 'saturation "0000ff" "0.43"))
    (test
      "15.02.03: saturation/0000ff/0.6"
      0
      (compare-image 'saturation "0000ff" "0.6"))
    (test
      "15.02.04: saturation/0000ff/0.78"
      0
      (compare-image 'saturation "0000ff" "0.78"))
    (test
      "15.02.05: saturation/0000ff/1.0"
      0
      (compare-image 'saturation "0000ff" "1.0"))
  )
  (test-group "[15.03] saturation/00ff00"
    (test
      "15.03.01: saturation/00ff00/0.2"
      0
      (compare-image 'saturation "00ff00" "0.2"))
    (test
      "15.03.02: saturation/00ff00/0.43"
      0
      (compare-image 'saturation "00ff00" "0.43"))
    (test
      "15.03.03: saturation/00ff00/0.6"
      0
      (compare-image 'saturation "00ff00" "0.6"))
    (test
      "15.03.04: saturation/00ff00/0.78"
      0
      (compare-image 'saturation "00ff00" "0.78"))
    (test
      "15.03.05: saturation/00ff00/1.0"
      0
      (compare-image 'saturation "00ff00" "1.0"))
  )
  (test-group "[15.04] saturation/27249c"
    (test
      "15.04.01: saturation/27249c/0.2"
      0
      (compare-image 'saturation "27249c" "0.2"))
    (test
      "15.04.02: saturation/27249c/0.43"
      0
      (compare-image 'saturation "27249c" "0.43"))
    (test
      "15.04.03: saturation/27249c/0.6"
      0
      (compare-image 'saturation "27249c" "0.6"))
    (test
      "15.04.04: saturation/27249c/0.78"
      0
      (compare-image 'saturation "27249c" "0.78"))
    (test
      "15.04.05: saturation/27249c/1.0"
      0
      (compare-image 'saturation "27249c" "1.0"))
  )
  (test-group "[15.05] saturation/54fa0d"
    (test
      "15.05.01: saturation/54fa0d/0.2"
      0
      (compare-image 'saturation "54fa0d" "0.2"))
    (test
      "15.05.02: saturation/54fa0d/0.43"
      0
      (compare-image 'saturation "54fa0d" "0.43"))
    (test
      "15.05.03: saturation/54fa0d/0.6"
      0
      (compare-image 'saturation "54fa0d" "0.6"))
    (test
      "15.05.04: saturation/54fa0d/0.78"
      0
      (compare-image 'saturation "54fa0d" "0.78"))
    (test
      "15.05.05: saturation/54fa0d/1.0"
      0
      (compare-image 'saturation "54fa0d" "1.0"))
  )
  (test-group "[15.06] saturation/5c5c5c"
    (test
      "15.06.01: saturation/5c5c5c/0.2"
      0
      (compare-image 'saturation "5c5c5c" "0.2"))
    (test
      "15.06.02: saturation/5c5c5c/0.43"
      0
      (compare-image 'saturation "5c5c5c" "0.43"))
    (test
      "15.06.03: saturation/5c5c5c/0.6"
      0
      (compare-image 'saturation "5c5c5c" "0.6"))
    (test
      "15.06.04: saturation/5c5c5c/0.78"
      0
      (compare-image 'saturation "5c5c5c" "0.78"))
    (test
      "15.06.05: saturation/5c5c5c/1.0"
      0
      (compare-image 'saturation "5c5c5c" "1.0"))
  )
  (test-group "[15.07] saturation/775acf"
    (test
      "15.07.01: saturation/775acf/0.2"
      0
      (compare-image 'saturation "775acf" "0.2"))
    (test
      "15.07.02: saturation/775acf/0.43"
      0
      (compare-image 'saturation "775acf" "0.43"))
    (test
      "15.07.03: saturation/775acf/0.6"
      0
      (compare-image 'saturation "775acf" "0.6"))
    (test
      "15.07.04: saturation/775acf/0.78"
      0
      (compare-image 'saturation "775acf" "0.78"))
    (test
      "15.07.05: saturation/775acf/1.0"
      0
      (compare-image 'saturation "775acf" "1.0"))
  )
  (test-group "[15.08] saturation/7f7d0a"
    (test
      "15.08.01: saturation/7f7d0a/0.2"
      0
      (compare-image 'saturation "7f7d0a" "0.2"))
    (test
      "15.08.02: saturation/7f7d0a/0.43"
      0
      (compare-image 'saturation "7f7d0a" "0.43"))
    (test
      "15.08.03: saturation/7f7d0a/0.6"
      0
      (compare-image 'saturation "7f7d0a" "0.6"))
    (test
      "15.08.04: saturation/7f7d0a/0.78"
      0
      (compare-image 'saturation "7f7d0a" "0.78"))
    (test
      "15.08.05: saturation/7f7d0a/1.0"
      0
      (compare-image 'saturation "7f7d0a" "1.0"))
  )
  (test-group "[15.09] saturation/808080"
    (test
      "15.09.01: saturation/808080/0.2"
      0
      (compare-image 'saturation "808080" "0.2"))
    (test
      "15.09.02: saturation/808080/0.43"
      0
      (compare-image 'saturation "808080" "0.43"))
    (test
      "15.09.03: saturation/808080/0.6"
      0
      (compare-image 'saturation "808080" "0.6"))
    (test
      "15.09.04: saturation/808080/0.78"
      0
      (compare-image 'saturation "808080" "0.78"))
    (test
      "15.09.05: saturation/808080/1.0"
      0
      (compare-image 'saturation "808080" "1.0"))
  )
  (test-group "[15.10] saturation/899675"
    (test
      "15.10.01: saturation/899675/0.2"
      0
      (compare-image 'saturation "899675" "0.2"))
    (test
      "15.10.02: saturation/899675/0.43"
      0
      (compare-image 'saturation "899675" "0.43"))
    (test
      "15.10.03: saturation/899675/0.6"
      0
      (compare-image 'saturation "899675" "0.6"))
    (test
      "15.10.04: saturation/899675/0.78"
      0
      (compare-image 'saturation "899675" "0.78"))
    (test
      "15.10.05: saturation/899675/1.0"
      0
      (compare-image 'saturation "899675" "1.0"))
  )
  (test-group "[15.11] saturation/98d5e4"
    (test
      "15.11.01: saturation/98d5e4/0.2"
      0
      (compare-image 'saturation "98d5e4" "0.2"))
    (test
      "15.11.02: saturation/98d5e4/0.43"
      0
      (compare-image 'saturation "98d5e4" "0.43"))
    (test
      "15.11.03: saturation/98d5e4/0.6"
      0
      (compare-image 'saturation "98d5e4" "0.6"))
    (test
      "15.11.04: saturation/98d5e4/0.78"
      0
      (compare-image 'saturation "98d5e4" "0.78"))
    (test
      "15.11.05: saturation/98d5e4/1.0"
      0
      (compare-image 'saturation "98d5e4" "1.0"))
  )
  (test-group "[15.12] saturation/b5b5b5"
    (test
      "15.12.01: saturation/b5b5b5/0.2"
      0
      (compare-image 'saturation "b5b5b5" "0.2"))
    (test
      "15.12.02: saturation/b5b5b5/0.43"
      0
      (compare-image 'saturation "b5b5b5" "0.43"))
    (test
      "15.12.03: saturation/b5b5b5/0.6"
      0
      (compare-image 'saturation "b5b5b5" "0.6"))
    (test
      "15.12.04: saturation/b5b5b5/0.78"
      0
      (compare-image 'saturation "b5b5b5" "0.78"))
    (test
      "15.12.05: saturation/b5b5b5/1.0"
      0
      (compare-image 'saturation "b5b5b5" "1.0"))
  )
  (test-group "[15.13] saturation/cd1f3c"
    (test
      "15.13.01: saturation/cd1f3c/0.2"
      0
      (compare-image 'saturation "cd1f3c" "0.2"))
    (test
      "15.13.02: saturation/cd1f3c/0.43"
      0
      (compare-image 'saturation "cd1f3c" "0.43"))
    (test
      "15.13.03: saturation/cd1f3c/0.6"
      0
      (compare-image 'saturation "cd1f3c" "0.6"))
    (test
      "15.13.04: saturation/cd1f3c/0.78"
      0
      (compare-image 'saturation "cd1f3c" "0.78"))
    (test
      "15.13.05: saturation/cd1f3c/1.0"
      0
      (compare-image 'saturation "cd1f3c" "1.0"))
  )
  (test-group "[15.14] saturation/f62db6"
    (test
      "15.14.01: saturation/f62db6/0.2"
      0
      (compare-image 'saturation "f62db6" "0.2"))
    (test
      "15.14.02: saturation/f62db6/0.43"
      0
      (compare-image 'saturation "f62db6" "0.43"))
    (test
      "15.14.03: saturation/f62db6/0.6"
      0
      (compare-image 'saturation "f62db6" "0.6"))
    (test
      "15.14.04: saturation/f62db6/0.78"
      0
      (compare-image 'saturation "f62db6" "0.78"))
    (test
      "15.14.05: saturation/f62db6/1.0"
      0
      (compare-image 'saturation "f62db6" "1.0"))
  )
  (test-group "[15.15] saturation/ff0000"
    (test
      "15.15.01: saturation/ff0000/0.2"
      0
      (compare-image 'saturation "ff0000" "0.2"))
    (test
      "15.15.02: saturation/ff0000/0.43"
      0
      (compare-image 'saturation "ff0000" "0.43"))
    (test
      "15.15.03: saturation/ff0000/0.6"
      0
      (compare-image 'saturation "ff0000" "0.6"))
    (test
      "15.15.04: saturation/ff0000/0.78"
      0
      (compare-image 'saturation "ff0000" "0.78"))
    (test
      "15.15.05: saturation/ff0000/1.0"
      0
      (compare-image 'saturation "ff0000" "1.0"))
  )
  (test-group "[15.16] saturation/ffffff"
    (test
      "15.16.01: saturation/ffffff/0.2"
      0
      (compare-image 'saturation "ffffff" "0.2"))
    (test
      "15.16.02: saturation/ffffff/0.43"
      0
      (compare-image 'saturation "ffffff" "0.43"))
    (test
      "15.16.03: saturation/ffffff/0.6"
      0
      (compare-image 'saturation "ffffff" "0.6"))
    (test
      "15.16.04: saturation/ffffff/0.78"
      0
      (compare-image 'saturation "ffffff" "0.78"))
    (test
      "15.16.05: saturation/ffffff/1.0"
      0
      (compare-image 'saturation "ffffff" "1.0"))
  )
)
(test-group "[16] screen"
  (test-group "[16.01] screen/000000"
    (test
      "16.01.01: screen/000000/0.2"
      0
      (compare-image 'screen "000000" "0.2"))
    (test
      "16.01.02: screen/000000/0.43"
      0
      (compare-image 'screen "000000" "0.43"))
    (test
      "16.01.03: screen/000000/0.6"
      0
      (compare-image 'screen "000000" "0.6"))
    (test
      "16.01.04: screen/000000/0.78"
      0
      (compare-image 'screen "000000" "0.78"))
    (test
      "16.01.05: screen/000000/1.0"
      0
      (compare-image 'screen "000000" "1.0"))
  )
  (test-group "[16.02] screen/0000ff"
    (test
      "16.02.01: screen/0000ff/0.2"
      0
      (compare-image 'screen "0000ff" "0.2"))
    (test
      "16.02.02: screen/0000ff/0.43"
      0
      (compare-image 'screen "0000ff" "0.43"))
    (test
      "16.02.03: screen/0000ff/0.6"
      0
      (compare-image 'screen "0000ff" "0.6"))
    (test
      "16.02.04: screen/0000ff/0.78"
      0
      (compare-image 'screen "0000ff" "0.78"))
    (test
      "16.02.05: screen/0000ff/1.0"
      0
      (compare-image 'screen "0000ff" "1.0"))
  )
  (test-group "[16.03] screen/00ff00"
    (test
      "16.03.01: screen/00ff00/0.2"
      0
      (compare-image 'screen "00ff00" "0.2"))
    (test
      "16.03.02: screen/00ff00/0.43"
      0
      (compare-image 'screen "00ff00" "0.43"))
    (test
      "16.03.03: screen/00ff00/0.6"
      0
      (compare-image 'screen "00ff00" "0.6"))
    (test
      "16.03.04: screen/00ff00/0.78"
      0
      (compare-image 'screen "00ff00" "0.78"))
    (test
      "16.03.05: screen/00ff00/1.0"
      0
      (compare-image 'screen "00ff00" "1.0"))
  )
  (test-group "[16.04] screen/27249c"
    (test
      "16.04.01: screen/27249c/0.2"
      0
      (compare-image 'screen "27249c" "0.2"))
    (test
      "16.04.02: screen/27249c/0.43"
      0
      (compare-image 'screen "27249c" "0.43"))
    (test
      "16.04.03: screen/27249c/0.6"
      0
      (compare-image 'screen "27249c" "0.6"))
    (test
      "16.04.04: screen/27249c/0.78"
      0
      (compare-image 'screen "27249c" "0.78"))
    (test
      "16.04.05: screen/27249c/1.0"
      0
      (compare-image 'screen "27249c" "1.0"))
  )
  (test-group "[16.05] screen/54fa0d"
    (test
      "16.05.01: screen/54fa0d/0.2"
      0
      (compare-image 'screen "54fa0d" "0.2"))
    (test
      "16.05.02: screen/54fa0d/0.43"
      0
      (compare-image 'screen "54fa0d" "0.43"))
    (test
      "16.05.03: screen/54fa0d/0.6"
      0
      (compare-image 'screen "54fa0d" "0.6"))
    (test
      "16.05.04: screen/54fa0d/0.78"
      0
      (compare-image 'screen "54fa0d" "0.78"))
    (test
      "16.05.05: screen/54fa0d/1.0"
      0
      (compare-image 'screen "54fa0d" "1.0"))
  )
  (test-group "[16.06] screen/5c5c5c"
    (test
      "16.06.01: screen/5c5c5c/0.2"
      0
      (compare-image 'screen "5c5c5c" "0.2"))
    (test
      "16.06.02: screen/5c5c5c/0.43"
      0
      (compare-image 'screen "5c5c5c" "0.43"))
    (test
      "16.06.03: screen/5c5c5c/0.6"
      0
      (compare-image 'screen "5c5c5c" "0.6"))
    (test
      "16.06.04: screen/5c5c5c/0.78"
      0
      (compare-image 'screen "5c5c5c" "0.78"))
    (test
      "16.06.05: screen/5c5c5c/1.0"
      0
      (compare-image 'screen "5c5c5c" "1.0"))
  )
  (test-group "[16.07] screen/775acf"
    (test
      "16.07.01: screen/775acf/0.2"
      0
      (compare-image 'screen "775acf" "0.2"))
    (test
      "16.07.02: screen/775acf/0.43"
      0
      (compare-image 'screen "775acf" "0.43"))
    (test
      "16.07.03: screen/775acf/0.6"
      0
      (compare-image 'screen "775acf" "0.6"))
    (test
      "16.07.04: screen/775acf/0.78"
      0
      (compare-image 'screen "775acf" "0.78"))
    (test
      "16.07.05: screen/775acf/1.0"
      0
      (compare-image 'screen "775acf" "1.0"))
  )
  (test-group "[16.08] screen/7f7d0a"
    (test
      "16.08.01: screen/7f7d0a/0.2"
      0
      (compare-image 'screen "7f7d0a" "0.2"))
    (test
      "16.08.02: screen/7f7d0a/0.43"
      0
      (compare-image 'screen "7f7d0a" "0.43"))
    (test
      "16.08.03: screen/7f7d0a/0.6"
      0
      (compare-image 'screen "7f7d0a" "0.6"))
    (test
      "16.08.04: screen/7f7d0a/0.78"
      0
      (compare-image 'screen "7f7d0a" "0.78"))
    (test
      "16.08.05: screen/7f7d0a/1.0"
      0
      (compare-image 'screen "7f7d0a" "1.0"))
  )
  (test-group "[16.09] screen/808080"
    (test
      "16.09.01: screen/808080/0.2"
      0
      (compare-image 'screen "808080" "0.2"))
    (test
      "16.09.02: screen/808080/0.43"
      0
      (compare-image 'screen "808080" "0.43"))
    (test
      "16.09.03: screen/808080/0.6"
      0
      (compare-image 'screen "808080" "0.6"))
    (test
      "16.09.04: screen/808080/0.78"
      0
      (compare-image 'screen "808080" "0.78"))
    (test
      "16.09.05: screen/808080/1.0"
      0
      (compare-image 'screen "808080" "1.0"))
  )
  (test-group "[16.10] screen/899675"
    (test
      "16.10.01: screen/899675/0.2"
      0
      (compare-image 'screen "899675" "0.2"))
    (test
      "16.10.02: screen/899675/0.43"
      0
      (compare-image 'screen "899675" "0.43"))
    (test
      "16.10.03: screen/899675/0.6"
      0
      (compare-image 'screen "899675" "0.6"))
    (test
      "16.10.04: screen/899675/0.78"
      0
      (compare-image 'screen "899675" "0.78"))
    (test
      "16.10.05: screen/899675/1.0"
      0
      (compare-image 'screen "899675" "1.0"))
  )
  (test-group "[16.11] screen/98d5e4"
    (test
      "16.11.01: screen/98d5e4/0.2"
      0
      (compare-image 'screen "98d5e4" "0.2"))
    (test
      "16.11.02: screen/98d5e4/0.43"
      0
      (compare-image 'screen "98d5e4" "0.43"))
    (test
      "16.11.03: screen/98d5e4/0.6"
      0
      (compare-image 'screen "98d5e4" "0.6"))
    (test
      "16.11.04: screen/98d5e4/0.78"
      0
      (compare-image 'screen "98d5e4" "0.78"))
    (test
      "16.11.05: screen/98d5e4/1.0"
      0
      (compare-image 'screen "98d5e4" "1.0"))
  )
  (test-group "[16.12] screen/b5b5b5"
    (test
      "16.12.01: screen/b5b5b5/0.2"
      0
      (compare-image 'screen "b5b5b5" "0.2"))
    (test
      "16.12.02: screen/b5b5b5/0.43"
      0
      (compare-image 'screen "b5b5b5" "0.43"))
    (test
      "16.12.03: screen/b5b5b5/0.6"
      0
      (compare-image 'screen "b5b5b5" "0.6"))
    (test
      "16.12.04: screen/b5b5b5/0.78"
      0
      (compare-image 'screen "b5b5b5" "0.78"))
    (test
      "16.12.05: screen/b5b5b5/1.0"
      0
      (compare-image 'screen "b5b5b5" "1.0"))
  )
  (test-group "[16.13] screen/cd1f3c"
    (test
      "16.13.01: screen/cd1f3c/0.2"
      0
      (compare-image 'screen "cd1f3c" "0.2"))
    (test
      "16.13.02: screen/cd1f3c/0.43"
      0
      (compare-image 'screen "cd1f3c" "0.43"))
    (test
      "16.13.03: screen/cd1f3c/0.6"
      0
      (compare-image 'screen "cd1f3c" "0.6"))
    (test
      "16.13.04: screen/cd1f3c/0.78"
      0
      (compare-image 'screen "cd1f3c" "0.78"))
    (test
      "16.13.05: screen/cd1f3c/1.0"
      0
      (compare-image 'screen "cd1f3c" "1.0"))
  )
  (test-group "[16.14] screen/f62db6"
    (test
      "16.14.01: screen/f62db6/0.2"
      0
      (compare-image 'screen "f62db6" "0.2"))
    (test
      "16.14.02: screen/f62db6/0.43"
      0
      (compare-image 'screen "f62db6" "0.43"))
    (test
      "16.14.03: screen/f62db6/0.6"
      0
      (compare-image 'screen "f62db6" "0.6"))
    (test
      "16.14.04: screen/f62db6/0.78"
      0
      (compare-image 'screen "f62db6" "0.78"))
    (test
      "16.14.05: screen/f62db6/1.0"
      0
      (compare-image 'screen "f62db6" "1.0"))
  )
  (test-group "[16.15] screen/ff0000"
    (test
      "16.15.01: screen/ff0000/0.2"
      0
      (compare-image 'screen "ff0000" "0.2"))
    (test
      "16.15.02: screen/ff0000/0.43"
      0
      (compare-image 'screen "ff0000" "0.43"))
    (test
      "16.15.03: screen/ff0000/0.6"
      0
      (compare-image 'screen "ff0000" "0.6"))
    (test
      "16.15.04: screen/ff0000/0.78"
      0
      (compare-image 'screen "ff0000" "0.78"))
    (test
      "16.15.05: screen/ff0000/1.0"
      0
      (compare-image 'screen "ff0000" "1.0"))
  )
  (test-group "[16.16] screen/ffffff"
    (test
      "16.16.01: screen/ffffff/0.2"
      0
      (compare-image 'screen "ffffff" "0.2"))
    (test
      "16.16.02: screen/ffffff/0.43"
      0
      (compare-image 'screen "ffffff" "0.43"))
    (test
      "16.16.03: screen/ffffff/0.6"
      0
      (compare-image 'screen "ffffff" "0.6"))
    (test
      "16.16.04: screen/ffffff/0.78"
      0
      (compare-image 'screen "ffffff" "0.78"))
    (test
      "16.16.05: screen/ffffff/1.0"
      0
      (compare-image 'screen "ffffff" "1.0"))
  )
)
(test-group "[17] subtract"
  (test-group "[17.01] subtract/000000"
    (test
      "17.01.01: subtract/000000/0.2"
      0
      (compare-image 'subtract "000000" "0.2"))
    (test
      "17.01.02: subtract/000000/0.43"
      0
      (compare-image 'subtract "000000" "0.43"))
    (test
      "17.01.03: subtract/000000/0.6"
      0
      (compare-image 'subtract "000000" "0.6"))
    (test
      "17.01.04: subtract/000000/0.78"
      0
      (compare-image 'subtract "000000" "0.78"))
    (test
      "17.01.05: subtract/000000/1.0"
      0
      (compare-image 'subtract "000000" "1.0"))
  )
  (test-group "[17.02] subtract/0000ff"
    (test
      "17.02.01: subtract/0000ff/0.2"
      0
      (compare-image 'subtract "0000ff" "0.2"))
    (test
      "17.02.02: subtract/0000ff/0.43"
      0
      (compare-image 'subtract "0000ff" "0.43"))
    (test
      "17.02.03: subtract/0000ff/0.6"
      0
      (compare-image 'subtract "0000ff" "0.6"))
    (test
      "17.02.04: subtract/0000ff/0.78"
      0
      (compare-image 'subtract "0000ff" "0.78"))
    (test
      "17.02.05: subtract/0000ff/1.0"
      0
      (compare-image 'subtract "0000ff" "1.0"))
  )
  (test-group "[17.03] subtract/00ff00"
    (test
      "17.03.01: subtract/00ff00/0.2"
      0
      (compare-image 'subtract "00ff00" "0.2"))
    (test
      "17.03.02: subtract/00ff00/0.43"
      0
      (compare-image 'subtract "00ff00" "0.43"))
    (test
      "17.03.03: subtract/00ff00/0.6"
      0
      (compare-image 'subtract "00ff00" "0.6"))
    (test
      "17.03.04: subtract/00ff00/0.78"
      0
      (compare-image 'subtract "00ff00" "0.78"))
    (test
      "17.03.05: subtract/00ff00/1.0"
      0
      (compare-image 'subtract "00ff00" "1.0"))
  )
  (test-group "[17.04] subtract/27249c"
    (test
      "17.04.01: subtract/27249c/0.2"
      0
      (compare-image 'subtract "27249c" "0.2"))
    (test
      "17.04.02: subtract/27249c/0.43"
      0
      (compare-image 'subtract "27249c" "0.43"))
    (test
      "17.04.03: subtract/27249c/0.6"
      0
      (compare-image 'subtract "27249c" "0.6"))
    (test
      "17.04.04: subtract/27249c/0.78"
      0
      (compare-image 'subtract "27249c" "0.78"))
    (test
      "17.04.05: subtract/27249c/1.0"
      0
      (compare-image 'subtract "27249c" "1.0"))
  )
  (test-group "[17.05] subtract/54fa0d"
    (test
      "17.05.01: subtract/54fa0d/0.2"
      0
      (compare-image 'subtract "54fa0d" "0.2"))
    (test
      "17.05.02: subtract/54fa0d/0.43"
      0
      (compare-image 'subtract "54fa0d" "0.43"))
    (test
      "17.05.03: subtract/54fa0d/0.6"
      0
      (compare-image 'subtract "54fa0d" "0.6"))
    (test
      "17.05.04: subtract/54fa0d/0.78"
      0
      (compare-image 'subtract "54fa0d" "0.78"))
    (test
      "17.05.05: subtract/54fa0d/1.0"
      0
      (compare-image 'subtract "54fa0d" "1.0"))
  )
  (test-group "[17.06] subtract/5c5c5c"
    (test
      "17.06.01: subtract/5c5c5c/0.2"
      0
      (compare-image 'subtract "5c5c5c" "0.2"))
    (test
      "17.06.02: subtract/5c5c5c/0.43"
      0
      (compare-image 'subtract "5c5c5c" "0.43"))
    (test
      "17.06.03: subtract/5c5c5c/0.6"
      0
      (compare-image 'subtract "5c5c5c" "0.6"))
    (test
      "17.06.04: subtract/5c5c5c/0.78"
      0
      (compare-image 'subtract "5c5c5c" "0.78"))
    (test
      "17.06.05: subtract/5c5c5c/1.0"
      0
      (compare-image 'subtract "5c5c5c" "1.0"))
  )
  (test-group "[17.07] subtract/775acf"
    (test
      "17.07.01: subtract/775acf/0.2"
      0
      (compare-image 'subtract "775acf" "0.2"))
    (test
      "17.07.02: subtract/775acf/0.43"
      0
      (compare-image 'subtract "775acf" "0.43"))
    (test
      "17.07.03: subtract/775acf/0.6"
      0
      (compare-image 'subtract "775acf" "0.6"))
    (test
      "17.07.04: subtract/775acf/0.78"
      0
      (compare-image 'subtract "775acf" "0.78"))
    (test
      "17.07.05: subtract/775acf/1.0"
      0
      (compare-image 'subtract "775acf" "1.0"))
  )
  (test-group "[17.08] subtract/7f7d0a"
    (test
      "17.08.01: subtract/7f7d0a/0.2"
      0
      (compare-image 'subtract "7f7d0a" "0.2"))
    (test
      "17.08.02: subtract/7f7d0a/0.43"
      0
      (compare-image 'subtract "7f7d0a" "0.43"))
    (test
      "17.08.03: subtract/7f7d0a/0.6"
      0
      (compare-image 'subtract "7f7d0a" "0.6"))
    (test
      "17.08.04: subtract/7f7d0a/0.78"
      0
      (compare-image 'subtract "7f7d0a" "0.78"))
    (test
      "17.08.05: subtract/7f7d0a/1.0"
      0
      (compare-image 'subtract "7f7d0a" "1.0"))
  )
  (test-group "[17.09] subtract/808080"
    (test
      "17.09.01: subtract/808080/0.2"
      0
      (compare-image 'subtract "808080" "0.2"))
    (test
      "17.09.02: subtract/808080/0.43"
      0
      (compare-image 'subtract "808080" "0.43"))
    (test
      "17.09.03: subtract/808080/0.6"
      0
      (compare-image 'subtract "808080" "0.6"))
    (test
      "17.09.04: subtract/808080/0.78"
      0
      (compare-image 'subtract "808080" "0.78"))
    (test
      "17.09.05: subtract/808080/1.0"
      0
      (compare-image 'subtract "808080" "1.0"))
  )
  (test-group "[17.10] subtract/899675"
    (test
      "17.10.01: subtract/899675/0.2"
      0
      (compare-image 'subtract "899675" "0.2"))
    (test
      "17.10.02: subtract/899675/0.43"
      0
      (compare-image 'subtract "899675" "0.43"))
    (test
      "17.10.03: subtract/899675/0.6"
      0
      (compare-image 'subtract "899675" "0.6"))
    (test
      "17.10.04: subtract/899675/0.78"
      0
      (compare-image 'subtract "899675" "0.78"))
    (test
      "17.10.05: subtract/899675/1.0"
      0
      (compare-image 'subtract "899675" "1.0"))
  )
  (test-group "[17.11] subtract/98d5e4"
    (test
      "17.11.01: subtract/98d5e4/0.2"
      0
      (compare-image 'subtract "98d5e4" "0.2"))
    (test
      "17.11.02: subtract/98d5e4/0.43"
      0
      (compare-image 'subtract "98d5e4" "0.43"))
    (test
      "17.11.03: subtract/98d5e4/0.6"
      0
      (compare-image 'subtract "98d5e4" "0.6"))
    (test
      "17.11.04: subtract/98d5e4/0.78"
      0
      (compare-image 'subtract "98d5e4" "0.78"))
    (test
      "17.11.05: subtract/98d5e4/1.0"
      0
      (compare-image 'subtract "98d5e4" "1.0"))
  )
  (test-group "[17.12] subtract/b5b5b5"
    (test
      "17.12.01: subtract/b5b5b5/0.2"
      0
      (compare-image 'subtract "b5b5b5" "0.2"))
    (test
      "17.12.02: subtract/b5b5b5/0.43"
      0
      (compare-image 'subtract "b5b5b5" "0.43"))
    (test
      "17.12.03: subtract/b5b5b5/0.6"
      0
      (compare-image 'subtract "b5b5b5" "0.6"))
    (test
      "17.12.04: subtract/b5b5b5/0.78"
      0
      (compare-image 'subtract "b5b5b5" "0.78"))
    (test
      "17.12.05: subtract/b5b5b5/1.0"
      0
      (compare-image 'subtract "b5b5b5" "1.0"))
  )
  (test-group "[17.13] subtract/cd1f3c"
    (test
      "17.13.01: subtract/cd1f3c/0.2"
      0
      (compare-image 'subtract "cd1f3c" "0.2"))
    (test
      "17.13.02: subtract/cd1f3c/0.43"
      0
      (compare-image 'subtract "cd1f3c" "0.43"))
    (test
      "17.13.03: subtract/cd1f3c/0.6"
      0
      (compare-image 'subtract "cd1f3c" "0.6"))
    (test
      "17.13.04: subtract/cd1f3c/0.78"
      0
      (compare-image 'subtract "cd1f3c" "0.78"))
    (test
      "17.13.05: subtract/cd1f3c/1.0"
      0
      (compare-image 'subtract "cd1f3c" "1.0"))
  )
  (test-group "[17.14] subtract/f62db6"
    (test
      "17.14.01: subtract/f62db6/0.2"
      0
      (compare-image 'subtract "f62db6" "0.2"))
    (test
      "17.14.02: subtract/f62db6/0.43"
      0
      (compare-image 'subtract "f62db6" "0.43"))
    (test
      "17.14.03: subtract/f62db6/0.6"
      0
      (compare-image 'subtract "f62db6" "0.6"))
    (test
      "17.14.04: subtract/f62db6/0.78"
      0
      (compare-image 'subtract "f62db6" "0.78"))
    (test
      "17.14.05: subtract/f62db6/1.0"
      0
      (compare-image 'subtract "f62db6" "1.0"))
  )
  (test-group "[17.15] subtract/ff0000"
    (test
      "17.15.01: subtract/ff0000/0.2"
      0
      (compare-image 'subtract "ff0000" "0.2"))
    (test
      "17.15.02: subtract/ff0000/0.43"
      0
      (compare-image 'subtract "ff0000" "0.43"))
    (test
      "17.15.03: subtract/ff0000/0.6"
      0
      (compare-image 'subtract "ff0000" "0.6"))
    (test
      "17.15.04: subtract/ff0000/0.78"
      0
      (compare-image 'subtract "ff0000" "0.78"))
    (test
      "17.15.05: subtract/ff0000/1.0"
      0
      (compare-image 'subtract "ff0000" "1.0"))
  )
  (test-group "[17.16] subtract/ffffff"
    (test
      "17.16.01: subtract/ffffff/0.2"
      0
      (compare-image 'subtract "ffffff" "0.2"))
    (test
      "17.16.02: subtract/ffffff/0.43"
      0
      (compare-image 'subtract "ffffff" "0.43"))
    (test
      "17.16.03: subtract/ffffff/0.6"
      0
      (compare-image 'subtract "ffffff" "0.6"))
    (test
      "17.16.04: subtract/ffffff/0.78"
      0
      (compare-image 'subtract "ffffff" "0.78"))
    (test
      "17.16.05: subtract/ffffff/1.0"
      0
      (compare-image 'subtract "ffffff" "1.0"))
  )
)
(test-group "[18] value"
  (test-group "[18.01] value/000000"
    (test
      "18.01.01: value/000000/0.2"
      0
      (compare-image 'value "000000" "0.2"))
    (test
      "18.01.02: value/000000/0.43"
      0
      (compare-image 'value "000000" "0.43"))
    (test
      "18.01.03: value/000000/0.6"
      0
      (compare-image 'value "000000" "0.6"))
    (test
      "18.01.04: value/000000/0.78"
      0
      (compare-image 'value "000000" "0.78"))
    (test
      "18.01.05: value/000000/1.0"
      0
      (compare-image 'value "000000" "1.0"))
  )
  (test-group "[18.02] value/0000ff"
    (test
      "18.02.01: value/0000ff/0.2"
      0
      (compare-image 'value "0000ff" "0.2"))
    (test
      "18.02.02: value/0000ff/0.43"
      0
      (compare-image 'value "0000ff" "0.43"))
    (test
      "18.02.03: value/0000ff/0.6"
      0
      (compare-image 'value "0000ff" "0.6"))
    (test
      "18.02.04: value/0000ff/0.78"
      0
      (compare-image 'value "0000ff" "0.78"))
    (test
      "18.02.05: value/0000ff/1.0"
      0
      (compare-image 'value "0000ff" "1.0"))
  )
  (test-group "[18.03] value/00ff00"
    (test
      "18.03.01: value/00ff00/0.2"
      0
      (compare-image 'value "00ff00" "0.2"))
    (test
      "18.03.02: value/00ff00/0.43"
      0
      (compare-image 'value "00ff00" "0.43"))
    (test
      "18.03.03: value/00ff00/0.6"
      0
      (compare-image 'value "00ff00" "0.6"))
    (test
      "18.03.04: value/00ff00/0.78"
      0
      (compare-image 'value "00ff00" "0.78"))
    (test
      "18.03.05: value/00ff00/1.0"
      0
      (compare-image 'value "00ff00" "1.0"))
  )
  (test-group "[18.04] value/27249c"
    (test
      "18.04.01: value/27249c/0.2"
      0
      (compare-image 'value "27249c" "0.2"))
    (test
      "18.04.02: value/27249c/0.43"
      0
      (compare-image 'value "27249c" "0.43"))
    (test
      "18.04.03: value/27249c/0.6"
      0
      (compare-image 'value "27249c" "0.6"))
    (test
      "18.04.04: value/27249c/0.78"
      0
      (compare-image 'value "27249c" "0.78"))
    (test
      "18.04.05: value/27249c/1.0"
      0
      (compare-image 'value "27249c" "1.0"))
  )
  (test-group "[18.05] value/54fa0d"
    (test
      "18.05.01: value/54fa0d/0.2"
      0
      (compare-image 'value "54fa0d" "0.2"))
    (test
      "18.05.02: value/54fa0d/0.43"
      0
      (compare-image 'value "54fa0d" "0.43"))
    (test
      "18.05.03: value/54fa0d/0.6"
      0
      (compare-image 'value "54fa0d" "0.6"))
    (test
      "18.05.04: value/54fa0d/0.78"
      0
      (compare-image 'value "54fa0d" "0.78"))
    (test
      "18.05.05: value/54fa0d/1.0"
      0
      (compare-image 'value "54fa0d" "1.0"))
  )
  (test-group "[18.06] value/5c5c5c"
    (test
      "18.06.01: value/5c5c5c/0.2"
      0
      (compare-image 'value "5c5c5c" "0.2"))
    (test
      "18.06.02: value/5c5c5c/0.43"
      0
      (compare-image 'value "5c5c5c" "0.43"))
    (test
      "18.06.03: value/5c5c5c/0.6"
      0
      (compare-image 'value "5c5c5c" "0.6"))
    (test
      "18.06.04: value/5c5c5c/0.78"
      0
      (compare-image 'value "5c5c5c" "0.78"))
    (test
      "18.06.05: value/5c5c5c/1.0"
      0
      (compare-image 'value "5c5c5c" "1.0"))
  )
  (test-group "[18.07] value/775acf"
    (test
      "18.07.01: value/775acf/0.2"
      0
      (compare-image 'value "775acf" "0.2"))
    (test
      "18.07.02: value/775acf/0.43"
      0
      (compare-image 'value "775acf" "0.43"))
    (test
      "18.07.03: value/775acf/0.6"
      0
      (compare-image 'value "775acf" "0.6"))
    (test
      "18.07.04: value/775acf/0.78"
      0
      (compare-image 'value "775acf" "0.78"))
    (test
      "18.07.05: value/775acf/1.0"
      0
      (compare-image 'value "775acf" "1.0"))
  )
  (test-group "[18.08] value/7f7d0a"
    (test
      "18.08.01: value/7f7d0a/0.2"
      0
      (compare-image 'value "7f7d0a" "0.2"))
    (test
      "18.08.02: value/7f7d0a/0.43"
      0
      (compare-image 'value "7f7d0a" "0.43"))
    (test
      "18.08.03: value/7f7d0a/0.6"
      0
      (compare-image 'value "7f7d0a" "0.6"))
    (test
      "18.08.04: value/7f7d0a/0.78"
      0
      (compare-image 'value "7f7d0a" "0.78"))
    (test
      "18.08.05: value/7f7d0a/1.0"
      0
      (compare-image 'value "7f7d0a" "1.0"))
  )
  (test-group "[18.09] value/808080"
    (test
      "18.09.01: value/808080/0.2"
      0
      (compare-image 'value "808080" "0.2"))
    (test
      "18.09.02: value/808080/0.43"
      0
      (compare-image 'value "808080" "0.43"))
    (test
      "18.09.03: value/808080/0.6"
      0
      (compare-image 'value "808080" "0.6"))
    (test
      "18.09.04: value/808080/0.78"
      0
      (compare-image 'value "808080" "0.78"))
    (test
      "18.09.05: value/808080/1.0"
      0
      (compare-image 'value "808080" "1.0"))
  )
  (test-group "[18.10] value/899675"
    (test
      "18.10.01: value/899675/0.2"
      0
      (compare-image 'value "899675" "0.2"))
    (test
      "18.10.02: value/899675/0.43"
      0
      (compare-image 'value "899675" "0.43"))
    (test
      "18.10.03: value/899675/0.6"
      0
      (compare-image 'value "899675" "0.6"))
    (test
      "18.10.04: value/899675/0.78"
      0
      (compare-image 'value "899675" "0.78"))
    (test
      "18.10.05: value/899675/1.0"
      0
      (compare-image 'value "899675" "1.0"))
  )
  (test-group "[18.11] value/98d5e4"
    (test
      "18.11.01: value/98d5e4/0.2"
      0
      (compare-image 'value "98d5e4" "0.2"))
    (test
      "18.11.02: value/98d5e4/0.43"
      0
      (compare-image 'value "98d5e4" "0.43"))
    (test
      "18.11.03: value/98d5e4/0.6"
      0
      (compare-image 'value "98d5e4" "0.6"))
    (test
      "18.11.04: value/98d5e4/0.78"
      0
      (compare-image 'value "98d5e4" "0.78"))
    (test
      "18.11.05: value/98d5e4/1.0"
      0
      (compare-image 'value "98d5e4" "1.0"))
  )
  (test-group "[18.12] value/b5b5b5"
    (test
      "18.12.01: value/b5b5b5/0.2"
      0
      (compare-image 'value "b5b5b5" "0.2"))
    (test
      "18.12.02: value/b5b5b5/0.43"
      0
      (compare-image 'value "b5b5b5" "0.43"))
    (test
      "18.12.03: value/b5b5b5/0.6"
      0
      (compare-image 'value "b5b5b5" "0.6"))
    (test
      "18.12.04: value/b5b5b5/0.78"
      0
      (compare-image 'value "b5b5b5" "0.78"))
    (test
      "18.12.05: value/b5b5b5/1.0"
      0
      (compare-image 'value "b5b5b5" "1.0"))
  )
  (test-group "[18.13] value/cd1f3c"
    (test
      "18.13.01: value/cd1f3c/0.2"
      0
      (compare-image 'value "cd1f3c" "0.2"))
    (test
      "18.13.02: value/cd1f3c/0.43"
      0
      (compare-image 'value "cd1f3c" "0.43"))
    (test
      "18.13.03: value/cd1f3c/0.6"
      0
      (compare-image 'value "cd1f3c" "0.6"))
    (test
      "18.13.04: value/cd1f3c/0.78"
      0
      (compare-image 'value "cd1f3c" "0.78"))
    (test
      "18.13.05: value/cd1f3c/1.0"
      0
      (compare-image 'value "cd1f3c" "1.0"))
  )
  (test-group "[18.14] value/f62db6"
    (test
      "18.14.01: value/f62db6/0.2"
      0
      (compare-image 'value "f62db6" "0.2"))
    (test
      "18.14.02: value/f62db6/0.43"
      0
      (compare-image 'value "f62db6" "0.43"))
    (test
      "18.14.03: value/f62db6/0.6"
      0
      (compare-image 'value "f62db6" "0.6"))
    (test
      "18.14.04: value/f62db6/0.78"
      0
      (compare-image 'value "f62db6" "0.78"))
    (test
      "18.14.05: value/f62db6/1.0"
      0
      (compare-image 'value "f62db6" "1.0"))
  )
  (test-group "[18.15] value/ff0000"
    (test
      "18.15.01: value/ff0000/0.2"
      0
      (compare-image 'value "ff0000" "0.2"))
    (test
      "18.15.02: value/ff0000/0.43"
      0
      (compare-image 'value "ff0000" "0.43"))
    (test
      "18.15.03: value/ff0000/0.6"
      0
      (compare-image 'value "ff0000" "0.6"))
    (test
      "18.15.04: value/ff0000/0.78"
      0
      (compare-image 'value "ff0000" "0.78"))
    (test
      "18.15.05: value/ff0000/1.0"
      0
      (compare-image 'value "ff0000" "1.0"))
  )
  (test-group "[18.16] value/ffffff"
    (test
      "18.16.01: value/ffffff/0.2"
      0
      (compare-image 'value "ffffff" "0.2"))
    (test
      "18.16.02: value/ffffff/0.43"
      0
      (compare-image 'value "ffffff" "0.43"))
    (test
      "18.16.03: value/ffffff/0.6"
      0
      (compare-image 'value "ffffff" "0.6"))
    (test
      "18.16.04: value/ffffff/0.78"
      0
      (compare-image 'value "ffffff" "0.78"))
    (test
      "18.16.05: value/ffffff/1.0"
      0
      (compare-image 'value "ffffff" "1.0"))
  )
)
