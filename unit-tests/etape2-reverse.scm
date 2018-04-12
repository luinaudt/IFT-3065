(define x '(1 2 3))
(define y '((4 . 5) "lala" (6 7 8)))
(println (reverse x))
(println (reverse y))
(println (reverse '((1 2 3) (4 5 6))))

;(3 2 1)
;((6 7 8) lala (4 . 5))
;((4 5 6) (1 2 3))
