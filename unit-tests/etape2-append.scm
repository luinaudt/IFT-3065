(define x '(1 2 3))
(define y '((4 . 5) "lala" (6 7 8)))
(println (append x y))
(println (append x '(4 5 6)))

;(1 2 3 (4 . 5) lala (6 7 8))
;(1 2 3 4 5 6)

