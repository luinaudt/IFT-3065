;;(define e (cons 5 6))
;;(println e)
(let ((e 5))
	(println ((lambda () (+ 123 e)))))
;123
