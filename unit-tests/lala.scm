(define x '(1 2 3))
(define y '(4 . 5))
(set-car! x 2)
(println x)
(set-car! x y)
(println x)

;(2 2 3)
;((4 . 5) 2 3)
