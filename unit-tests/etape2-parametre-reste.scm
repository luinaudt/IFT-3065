(define test (lambda (x . y)
	       (begin
		 (println x)
		 (println y))))

(define test2 (lambda (x . y)
	       (+ x
		  (let loop ((e y))
		    (if (not (null? e))
			(+ (car e) (loop (cdr e)))
			0)))))
(println (test2 5 6))
(println (test2 5))
(println (test2 9 10 11 12 6 7 8 9))
;11
;5
;72
