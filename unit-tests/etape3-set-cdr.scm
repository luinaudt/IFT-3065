(define x '(1 2 3))
(define y '(4 . 5))
(set-cdr! (cdr (cdr x)) 8)
(println x)
(set-cdr! x y)
(println x)

;(1 2 3 . 8)
;(1 4 . 5)
