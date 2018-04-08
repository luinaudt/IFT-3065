(define f
  (lambda (n)
    (lambda (x)
      ($+ n x))))

(println ((f 10) 5))

;15
