(define test
  (lambda (n)
    (cons (lambda (x)
	    (begin
	      (set! n (+ n x))
	      n))
	  (lambda (x)
	    (begin
	      (set! n (- n x))
	      n)))))

(define p (test 0))
(define inc (car p))
(define dec (cdr p))

(inc 3)
(inc 3)
(dec 1)
;3
;6
;5
