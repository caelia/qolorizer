(use qolorizer)
(use fastcgi)
(use files)

(define *image-path* (make-parameter #f))
(define *base-image-suffix* (make-parameter "base"))

(define (base-image-path filename)
  (make-pathname (make-pathname (*image-path*) (*base-image-suffix*)) filename))

(define (parse-colorized-image-path path)
  #f)

;; This procedure assumes that the requested file does not exist.
;; In my opinion, your web server should serve existing images and
;; delegate only requests for nonexistent images to this program.
(define (save-and-send path)
  #f)