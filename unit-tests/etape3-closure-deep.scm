(define test
  (lambda (x)
    (lambda (y)
      (lambda (z)
	(+ x (* z y))))))
(define test2
  (lambda (x1 x2)
    (lambda (y1 y2 y3)
      (lambda (z1)
	(+ (* x1 (quotient y2 y1)) (- x2 (modulo y3 z1)))))))

(println (((test 5) 7) 11))
(println (((test 7) 11) 3))
(println (((test2 3 5) 25 12 32) 21))
(println (((test2 55 3) 56 99 25) 89))


;82
;40
;-6
;33
