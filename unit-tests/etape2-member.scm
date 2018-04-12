(define equal? 0)
(println (member 2 '(1 2 3)))
(println (member '(2) '(1 3 2)))
(println (member '(2) '(1 5)))
(println (member '(2) '((1 2) (2 1))))
(println (member "bo" '("bonjour")))
(println (member "bo" '(1 2 "bo" 36)))
(println (member #t '(#f #f 5 #t)))
		 
;(2 3)
;#f
;#f
;#f
;#f
;(bo 36)
;(#t)
