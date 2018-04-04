(define f
  (lambda (n y)
    (lambda (x) ($+ n x))))
(define g (f 5 6))
(define h (f 42 43))
(println (g 10))
(println (+ 5 7))
