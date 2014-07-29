(use imlib2)

(with-output-to-file "rgbhsv-data.txt"
  (lambda ()
    (let ((img (image-load "rgbhsv.png")))
      (let hloop ((x 10))
        (when (< x 100)
          (let vloop ((y 10))
            (when (< y 80)
              (let-values (((r g b _) (image-pixel/rgba img x y))
                           ((h s v _) (image-pixel/hsva img x y)))
                (printf "[[~A, ~A, ~A], [~A, ~A, ~A]],\n" r g b h s v))
              (vloop (+ y 20))))
          (hloop (+ x 20)))))))
