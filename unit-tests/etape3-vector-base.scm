(define a (make-vector 10))
(println (vector? a))
(println (vector? "456"))
(vector-set! a 4 "456")
(println (vector-ref a 4))
(vector-set! a 5 '(4 5 6))
(println (vector-ref a 5))
;#t
;#f
;456
;(4 5 6)
