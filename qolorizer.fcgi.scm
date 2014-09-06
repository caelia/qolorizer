(use qolorizer)
(use fastcgi)
(use intarweb)
(use uri-common)
(use matchable)
(use files)

(define *image-path* (make-parameter #f))
(define *base-image-suffix* (make-parameter "base"))

(define (base-image-path filename)
  (make-pathname (make-pathname (*image-path*) (*base-image-suffix*)) filename))

(define (list->pathname segments)
  (unless (pair? segments)
    (error "Missing filename"))
  (foldl make-pathname (->string (car segments)) (cdr segments)))

(define (parse-colorized-image-path pathstr)
  (let ((segments (uri-path (uri-reference path))))
    (match segments
      [(mode color alpha . rest)
       (values (list->pathname segments)
               (string->symbol mode)
               color
               (string->number alpha)
               (list->pathname rest))]
      [_
       (error "Invalid path specification.")])))

;; This procedure assumes that the requested file does not exist.
;; In my opinion, your web server should serve existing images and
;; delegate only requests for nonexistent images to this program.
(define (save-and-send path)
  (let-values (((dest-path mode color alpha sub-path) (parse-colorized-image-path path)))
    (let ((base-path (base-image-path sub-path)))
      (colorize (mk-blend-op color blend-mode: mode alpha: alpha) base-path dest-path))))