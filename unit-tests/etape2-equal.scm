(define test +)
(println (equal? 1 1))
(println (equal? 1 2))
(println (equal? #f #t))
(println (equal? #t #t))
(println (equal? "" ""))
(println (equal? "bonjour" "bonjour"))
(println (equal? "bqw" "bb"))
(println (equal? + -))
(println (equal? + +))
(println (equal? + test))
(println (equal? '(1 2 3) '(1 2 3)))
(println (equal? '(1 2 3) '(1 2)))
(println (equal? '(1 2) '(1 2 3)))
(println (equal? '(1) 1))
(println (equal? '(5 . 6) '(5 . 6)))
(println (equal? '(5 . 6) '(5 6)))
(println (equal? '(5 . 6) '(5 . 7)))
(println (equal? '(5 (7 8)) '(5 (7 8))))
(println (equal? '(5 (7 9)) '(5 (7 8))))

;#t
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#f
;#t
;#f
;#f
;#t
;#f
