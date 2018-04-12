(define x 5)
(define b #t)
(define c #\a)
(define s "sup")
(define n '())
(define p '(1 . 2))
(define l '(1 2 3))
(define f println)
(println (car l))
(println (cdr l))
(println (list? x))
(println (list? b))
(println (list? c))
(println (list? s))
(println (list? n))
(println (list? p))
(println (list? l))
(println (list? f))

;1
;(2 3)
;#f
;#f
;#f
;#f
;#t
;#f
;#t
;#f
