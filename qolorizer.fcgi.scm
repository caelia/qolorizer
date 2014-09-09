(use qolorizer)
(use posix)
(use getopt-long)
(use fastcgi)
(use intarweb)
(use uri-common)
(use matchable)
(use files)
(use extras)

(define *image-path* (make-parameter #f))
(define *base-image-suffix* (make-parameter "base"))

(define (base-image-path filename)
  (make-pathname (make-pathname (*image-path*) (*base-image-suffix*)) filename))

(define (list->pathname segments)
  (unless (pair? segments)
    (error "Missing filename"))
  (foldl make-pathname (->string (car segments)) (cdr segments)))

(define (parse-colorized-image-path pathstr)
  (let ((segments (uri-path (uri-reference pathstr))))
    (match segments
      [(/ mode color alpha . rest)
       (values (list->pathname segments)
               (string->symbol mode)
               (string-append "#" color)
               (string->number alpha)
               (list->pathname rest))]
      [_
       (error "Invalid path specification.")])))

;; This procedure assumes that the requested file does not exist.
;; In my opinion, your web server should serve existing images and
;; delegate only requests for nonexistent images to this program.
(define (save-and-send path out)
  (fprintf (current-error-port) "Creating image '~A'" path)
  (let-values (((dest-path mode color alpha sub-path) (parse-colorized-image-path path)))
    (let ((base-path (base-image-path sub-path)))
      (colorize (mk-blend-op color blend-mode: mode alpha: alpha) base-path dest-path))
  (with-input-from-file
    dest-path
    (lambda ()
      (out (read-all)))
    #:binary))
  #t)
    
(define (handle-request in out err env)
  (let ((path (alist-ref "REQUEST_URI" (env) string=?)))
    (save-and-send path out)))
    
    
(define option-grammar
  '((tcp-port "The TCP port to listen on [default: 3429]"
              (value #t)
              (single-char #\t))
    (unix-socket "The Unix socket to listen on. If provided, this option
                  overrides [--tcp-port|-t]."
                 (value #t)
                 (single-char #\u))))
    
(define (start)
  (let* ((parsed-args (getopt-long (cdr (argv)) option-grammar))
         (rest (alist-ref '@ parsed-args))
         (image-path (if (null? rest) (current-directory) (car rest)))
         (unix-socket (alist-ref 'unix-socket parsed-args))
         (tcp-port-arg (alist-ref 'tcp-port parsed-args))
         (socket/port (or unix-socket
                          (if tcp-port-arg
                            (string->number tcp-port-arg)
                            3429))))
    (*image-path* image-path)
    (fcgi-accept-loop socket/port 0 handle-request)))
    
(start)