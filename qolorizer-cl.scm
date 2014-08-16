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
