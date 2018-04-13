(println (member 2 '()))
(println (member 2 '(1 2 3)))
(println (member 2 '(1 (2) 3)))
(println (member '(2) '(1 2 3)))
(println (member '(2) '((1 2) (2 1))))
(println (member '(2) '(1 (2) 3)))
(println (member "bo" '("bonjour")))
(println (member "bo" '(1 2 "bo" 36)))
(println (member #t '(#f #f 5 #t)))
(println (member #f '(1 2 3 #f)))
(println (member #f '(1 2 3 4)))

;#f
;(2 3)
;#f
;#f
;#f
;((2) 3)
;#f
;(bo 36)
;(#t)
;(#f)
;#f
