(define test (lambda (x . y)
	       (+ x
		  (let loop ((e y))
		    (if (not (null? e))
			(+ (car e) (loop (cdr e)))
			0)))))
(println (test 5))
(println (test 5 6 7 8 9))
;5
;35
