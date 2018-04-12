(define test
  (lambda (x)
    (lambda (y)
      (lambda (z)
	(+ x (* z y))))))
(println (((test 5) 7) 11))
(println (((test 7) 11) 3))

;82
;40
