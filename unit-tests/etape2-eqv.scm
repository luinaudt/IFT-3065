(define bonjour "5")
(define k bonjour)

(println (eqv? bonjour k))
(println (eqv? "ds" "g"))
(println (eqv? 5 5))
(println (eqv? 5 6))
(println (eqv? #t #t))
(println (eqv? #f #t))
(println (eqv? println +))
(println (eqv? '(5 6 7) '(5 6 7)))
(println (eqv? '() '()))

;#t
;#f
;#t
;#f
;#t
;#f
;#f
;#f
;#t
