(use qolorizer)
(use irregex)
(use posix)
(user srfi-1)
(use getopt-long)

(define *supported-extensions* (make-parameter '("png")))

(define (mk-dest-name pattern filename mode color alpha)
  (let-values (((dir base ext) (decompose-pathname filename)))
    (let ((dir (or dir ""))
          (base (or base ""))
          (ext (or ext "")))
      (make-pathname
        (irregex-replace/all "%a"
          (irregex-replace/all "%c"
            (irregex-replace/all "%m"
              (irregex-replace/all "%f"
                (irregex-replace/all "%d" pattern dir)
                base)
              mode)
            color)
          alpha)
          ext))))
  
(define (process-file src color-spec
                      #!key (blend-mode 'normal) (alpha 1.0) (dest #f) (dest-pattern #f))
  (let ((dest-file
          (if dest
            dest
            (let ((patt (or dest-pattern "%d/%f-%m-%c-%a")))
              (mk-dest-name patt src blend-mode color-spec alpha))))
        (blend (mk-blend-op color-spec blend-mode: blend-mode alpha: alpha)))
    (colorize blend src dest-file)))
  
(define (process-dir src-dir color-spec
                     #!key (blend-mode 'normal) (alpha 1.0) (dest #f) (dest-pattern #f))
  (let ((dest-dir
          (if dest
            dest
            (let ((patt (or dest-pattern "%m/%c/%a/%f")))
              (mk-dest-name patt src blend-mode color-spec alpha)))))
    (for-each
      (lambda (src-file)
        (let-values (((_ base ext) (decompose-pathname src-file)))
          (let ((dest-file (make-pathname dest base ext)))
            (process-file src-file color-spec blend-mode: blend-mode alpha: alpha))))
      (filter (lambda (f) (member (pathname-extension f) (*supported-extensions*)))
              (directory src-dir)))))

(define option-grammar
  `((output-file "The name of the output image file. This option is ignored if --output-dir is specified."
                 (value #t)
                 (single-char #\o))
    (input-dir "The directory where input files are located. All supported image files in this directory will be processed."
               (value #t)
               (single-char #\I))
    (output-dir "The directory where output files will be written."(value #t)
                (single-char #\O))
    (color "The RGB[A] color to apply to the image. The color may be specified in either of two formats: either a
            CSS-style hexadecimal color string such as '#c7de29' or a comma-separated list of decimal integers, such
            as '127,221,94'. The default color is '#000000' (black)."
           (value #t)
           (single-char #\c))
    (mode "The blend mode operator to apply [default=normal]. Supported values are:
              normal
              multiply 
              screen
              overlay 
              darken-only
              lighten-only 
              dodge
              burn 
              hard-light
              soft-light 
              difference
              exclusion 
              hue
              saturation 
              color
              luminosity
           See <http://cairographics.org/operators/> for more info."
          (value #t)
          (single-char #\m))
    (alpha "The alpha value of the color layer, expressed as a number from 0-1 [default=1.0]."
           (value #t)
           (single-char #\a)))))