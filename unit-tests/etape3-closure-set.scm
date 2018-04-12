(define test2
  (lambda (n)
    (lambda (x)
      (begin
	(set! n (+ n x))
	n))))

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

(define ts (test2 0))
(define p (test 0))
(define inc (car p))
(define dec (cdr p))

(println (ts  1))
(println (inc 3))
(println (inc 3))
(println (dec 1))
;1
;3
;6
;5
