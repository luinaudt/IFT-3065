(define x 5)
(define b #t)
(define c #\a)
(define s "sup")
(define n '())
(define p '(1 . 2))
(define l '(1 2 3))
(define f println)

(println (procedure? x))
(println (procedure? b))
(println (procedure? c))
(println (procedure? s))
(println (procedure? n))
(println (procedure? p))
(println (procedure? l))
(println (procedure? f))

;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
