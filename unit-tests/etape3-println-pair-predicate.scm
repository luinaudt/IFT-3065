(define x 5)
(define b #t)
(define c #\a)
(define s "sup")
(define n '())
(define p '(1 . 2))
(define l '(1 2 3))
(define f println)

(println (pair? x))
(println (pair? b))
(println (pair? c))
(println (pair? s))
(println (pair? n))
(println (pair? p))
(println (pair? l))
(println (pair? f))

;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#f
