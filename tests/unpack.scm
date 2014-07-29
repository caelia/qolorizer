(use z3)
(use base64)

(define (label->path label filename)
  (let loop ((segments (string-split label "-")))
    (if (null? segments)
      filename
      (if (null? (cdr segments))
        (make-pathname (car segments) filename)
        (make-pathname (car segments) (loop (cdr segments)))))))
  
(define (b64-decoder label filename)
  (let ((path (label->path label filename))
        (data ""))
    (lambda (msg . args)
      (case msg
        ((&)
          (set! data (string-append data (car args))))  
        ((*) 
          (create-directory (pathname-directory path) #t)
          (with-output-to-file
            path
            (lambda ()
              (base64-decode data (current-output-port)))))))))

(define (unpack b64-file outfile #!optional (start-dir #f))
  (let ((ip (z3:open-compressed-input-file b64-file))
        (prev-dir (current-directory)))
    (when start-dir
      (unless (file-exists? start-dir)
        (create-directory start-dir #t))
      (change-directory start-dir))
    (with-input-from-port
      ip
      (lambda ()
        (let loop ((line (read-line)) (dec #f))
          (cond
            ((eof-object? line)
              (if dec
                (dec '*)
                #f))
            ((eqv? (string-ref line 0) #\%)
              (when dec (dec '*))
              (loop (read-line) (b64-decoder (cadr (string-split line)) outfile)))
            (else
              (unless dec (error "Unexpected input: file corrupt?"))
              (dec '& line)
              (loop (read-line) dec))))))
    (close-input-port ip)
    (when start-dir (change-directory prev-dir))))
