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
          (cond
            (dest dest)
            (dest-pattern
              (let ((patt (or dest-pattern "%m/%c/%a/%f")))
                (mk-dest-name patt src blend-mode color-spec alpha)))
            (else (error "When processing a directory you must specify a destination directory name or pattern.")))))
    (for-each
      (lambda (src-file)
        (let-values (((_ base ext) (decompose-pathname src-file)))
          (let ((dest-file (make-pathname dest base ext)))
            (process-file src-file color-spec blend-mode: blend-mode alpha: alpha dest: dest-file))))
      (filter (lambda (f) (member (pathname-extension f) (*supported-extensions*)))
              (directory src-dir)))))

(define (usage)
  (printf "~A [options] [input_file]\n" (car (argv))))
              
(define option-grammar
  `((output-file "The name of the output image file. This option is ignored if --output-dir is specified."
                 (value #t)
                 (single-char #\o))
    (output-dir "The directory where output files will be written."
                (value #t)
                (single-char #\O))
    (file-pattern "A substitution pattern for output file names."
                  (value #t)
                  (single-char #\p))
    (dir-pattern "A substitution pattern for output dircectory names."
                 (value #t)
                 (single-char #\P))
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
           (single-char #\a))
    (help "Show this message."
          (single-char #\h))))
          
(define (cl-run)
  (let* ((args (getopt-long (argv) option-grammar))
         (output-file (alist-ref 'output-file args))
         (output-dir (alist-ref 'output-dir args))
         (input-dir (alist-ref 'input-dir args))
         (color (alist-ref 'color args))
         (mode (alist-ref 'mode args))
         (alpha (alist-ref 'alpha args))
         (help (alist-ref 'help args))
         (input-files (alist-ref '@ args)))
    (cond
      ((not (or input-dir input-files))
        (error "Please specify an input file or directory."))
      ((and input-dir input-files)
        (error "Please specify an input file OR directory, not both."))
      ((and output-dir output-files)
        (error "Please specify an output file OR directory, not both."))
      ((and output-file (> (length input-files) 1))
        (error "You may not specify a single output file with multiple input files."))
      (input-files
        (for-each
          (lambda (file)
            (let ((output-dir (or output-dir (pathname-directory file))))))
           (single-char #\a))))

(define (cl-run)
  (let* ((parsed-args (getopt-long (argv) option-grammar))
         (inputs (alist-ref '@ parsed-args))
         (output-file (alist-ref 'output-file parsed-args))
         (output-dir (alist-ref 'output-dir parsed-args))
         (file-pattern (alist-ref 'file-pattern parsed-args))
         (dir-pattern (alist-ref 'dir-pattern parsed-args))
         (color (alist-ref 'color parsed-args))
         (mode (or (alist-ref 'mode parsed-args) 'normal))
         (alpha-arg (alist-ref 'alpha parsed-args))
    (cond
      ((not inputs)
        (error "Please specify an input file or directory."))
      ((and output-dir output-files)
        (error "Please specify an output file OR directory, not both."))
      ((and output-file (> (length inputs) 1))
        (error "You may not specify a single output file with multiple input files."))
         (alpha (if alpha-arg (string->number alpha-arg) 1.0))
      (else
        (for-each
          (lambda (input)
            (cond
              ((not (file-exists? input))
                (error (sprintf "The input file '~A' does not exist.")))
              ((directory? input)
                (process-dir input color blend-mode: mode alpha: alpha dest: output-dir dest-pattern: dir-pattern))
              (else
                (process-file input color blend-mode: mode alpha: alpha dest: output-file dest-pattern: file-pattern))))
          inputs)))))

(cl-run)