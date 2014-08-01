(use qolorizer)
(use imlib2)
(use posix)

(define (do-ior infile outfile op)
  (let* ((src (image-load infile))
         (dest (colorize src op)))
    (image-save dest outfile)
    (image-destroy dest)
    (image-destroy src)))

(define (mk-destname src-file color)
  (pathname-replace-file
    src-file
    (string-append
      (pathname-file src-file)
      "-ior-"
      (list->string (cdr (string->list color))))))

(define (run)
  (let* ((color (cadr (argv)))
         (infiles (cddr (argv)))
         (op (mkop-rgb-ior color)))
    (for-each
      (lambda (fn)
        (do-ior fn (mk-destname fn color) op))
      infiles)))

(run)
