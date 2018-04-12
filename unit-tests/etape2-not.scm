(define x #t)
(define y #f)
(define z (lambda () (println 2)))
(println (not x))
(println (not y))
(println (not z))
(println (not #f))
(println (not #t))
(println (not 5))

;#f
;#t
;#f
;#t
;#f
;#f
