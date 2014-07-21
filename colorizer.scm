(module colorizer
        colorize
        
        (import scheme chicken)
        (use imlib2)

(define (blend-normal r0 g0 b0 a0 r1 g1 b1 a1) 
  (cond
    ((= a1 1) (values r1 g1 b1 1.0))
    ((= a1 0) (values r0 g0 b0 0.0))

(define (colorize src color #!key (blend-mode 'normal) (alpha 100))
  (let* ((width (image-width src))
         (height (image-height src))
         (dest* (image-create width height))
         (op
           (case blend-mode
             ((normal) blend-normal)
             ((multiply) blend-multiply)
             ((screen) blend-screen)
             ((dodge) blend-dodge)
             ((burn) blend-burn))
             (else (error (sprintf "Unknown blend mode: '~A'" blend-mode)))))
    (let loop-rows ((r 0))
      
      (let loop-cols ((c 0))

)
