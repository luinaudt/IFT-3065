(println (make-vector 5 3))
(println (make-vector 0 3))
(define t (make-vector 5 "456"))
(println t)
(string-set! (vector-ref t 0) 1 #\2)
(println t)


;#(3 3 3 3 3)
;#()
;#(456 456 456 456 456)
;#(426 426 426 426 426)
