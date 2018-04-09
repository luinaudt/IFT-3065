(define f
  (lambda (n y)
    (lambda (x) ($+ n x))))
;;(define g (f 5 6))
(define g2 (lambda (x) ((f 5 6) x)))
;;(define h (f 42 43))
(g2 10)
;;(println (g 10))





