(define x 5)
(define y '(1 2 . 3))
(println (set! x 10))
(println x)
(println (set-car! y 11))
(println (set-cdr! y '(22 33)))
(println y)

;#!void
;10
;#!void
;#!void
;(11 22 33)
