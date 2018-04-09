(define f
  (lambda (n y)
    (lambda (x) ($+ n x))))
;;(define g (f 5 6))
(define g2 (lambda (x) ((f 5 6) x)))
;;(define h (f 42 43))
(println (lambda (x) ((f 5 6 7) x)))
(println (g2 10))
;;(println (g 10))





