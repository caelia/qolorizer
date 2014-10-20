(use qolorizer)
(use posix)
(use getopt-long)
(use fastcgi)
(use intarweb)
(use uri-common)
(use matchable)
(use files)
(use utils)
(use extras)

(define *image-path* (make-parameter #f))
(define *base-image-suffix* (make-parameter "base"))
(define *save-image* (make-parameter #t))

(define (base-image-path filename)
  (make-pathname (make-pathname (*image-path*) (*base-image-suffix*)) filename))
  
(define (deslash path)
  (let ((chars (string->list path)))
    (if (eqv? (car chars) #\/)
      (list->string (cdr chars))
      path)))

(define (list->pathname segments)
  (unless (pair? segments)
    (error "Missing filename"))
  (foldl make-pathname (->string (car segments)) (cdr segments)))

(define (parse-colorized-image-path pathstr)
  (let ((segments (uri-path (uri-reference pathstr))))
    (match segments
      [(mode color alpha . rest)
       (list (list->pathname segments)
             (string->symbol mode)
             (string-append "#" color)
             (string->number alpha)
             (list->pathname rest))]
      [_
        #:invalid-path])))

(define (http-error out code)
  (let ((hdrs (headers '((status . ("Error"))))))
    (out
      (with-output-to-string
        (lambda ()
          (write-response (make-response port: (current-output-port) code: code)))))))

;; This procedure assumes that the requested file does not exist.
;; In my opinion, your web server should serve existing images and
;; delegate only requests for nonexistent images to this program.
(define (save-and-send path out env)
  (handle-exceptions
    _
    (http-error out 500)
    (let ((path-data (parse-colorized-image-path path)))
      (match path-data
        [(dest-path* mode color alpha sub-path)
          (let ((dest-path
                  (let ((save-image
                         (and (not (get-environment-variable "QOLORIZER_NO_SAVE"))
                              (not (env "QOLORIZER_NO_SAVE" #f))
                              (*save-image*))))
                    (if save-image
                      (let ((image-path
                             (or (get-environment-variable "QOLORIZER_IMAGE_PATH")
                                 (env "QOLORIZER_IMAGE_PATH" #f)
                                 (*image-path*))))
                        (make-pathname image-path dest-path*))
                      (create-temporary-file "png"))))
                (base-path
                  (base-image-path sub-path)))
            (if (file-exists? base-path)
              (begin
                (create-directory (pathname-directory dest-path) #t)
                ; The following is just for debugging purposes
                ; (display (string-append "Generated '" dest-path "'\n") (current-error-port)) 
                (colorize (mk-blend-op color blend-mode: mode alpha: alpha) base-path dest-path)
                (let* ((response-data
                        (with-input-from-file dest-path read-all #:binary))
                       (hdrs
                         (headers
                           `((content-type . ("image/png"))
                             (content-length . (,(string-length response-data)))))))
                  (out
                    (with-output-to-string
                      (lambda ()
                        (let ((resp (make-response port: (current-output-port) headers: hdrs)))
                          (write-response resp)
                          (display response-data)
                          (finish-response-body resp)))))))
              (http-error out 404)))]
        [#:invalid-path
          (http-error out 400)]
        [_
          (http-error out 500)])))
  #t)
    
(define (handle-request in out err env)
  (let ((path (alist-ref "REQUEST_URI" (env) string=?)))
    (save-and-send (deslash path) out env)))
    
   
(define option-grammar
  '((tcp-port "The TCP port to listen on [default: 3429]"
              (value #t)
              (single-char #\t))
    (unix-socket "The Unix socket to listen on. If provided, this option
                  overrides [--tcp-port|-t]."
                 (value #t)
                 (single-char #\u))
    (no-save "Don't save images; only send them in response to requests."
             (single-char #\n))))
    
(define (start)
  (let* ((parsed-args (getopt-long (cdr (argv)) option-grammar))
         (rest (alist-ref '@ parsed-args))
         ; Not totally sure about the logic here, but I am giving precedence to the environment
         ; var under the assumption that it will be passed in by an app container like uwsgi.
         (image-path
           (if (null? rest)
             (current-directory)
             (car rest)))
         (unix-socket-var
           (get-environment-variable "QOLORIZER_UNIX_SOCKET"))
         (unix-socket
           (if unix-socket-var
             unix-socket-var
             (alist-ref 'unix-socket parsed-args)))
         (tcp-port-var
           (get-environment-variable "QOLORIZER_TCP_PORT"))
         (tcp-port-arg
           (if tcp-port-var
             tcp-port-var
             (alist-ref 'tcp-port parsed-args)))
         (socket/port (or unix-socket
                          (if tcp-port-arg
                            (string->number tcp-port-arg)
                            3429)))
         (dont-save (alist-ref 'no-save parsed-args)))
    (when dont-save (*save-image* #f))
    (*image-path* image-path)
    (fcgi-accept-loop socket/port 0 handle-request)))
    
(start)
