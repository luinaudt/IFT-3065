(println (assoc 2 '()))
(println (assoc 2 '((1 11) (2 22) (3 33))))
(println (assoc 2 '(((1) 11) ((2) 22) ((3) 33))))
(println (assoc '(2) '((1 11) ((2 22) (3 33)))))
(println (assoc '(2) '((1 11) (2 22) (3 33))))
(println (assoc '(2) '(((1) 2) ((2) 1))))
(println (assoc "bo" '(("bonjour" #t))))
(println (assoc "bo" '((1 #t) (2 #t) ("bo" #t) (36 #f))))
(println (assoc #t '((#f 11) (#f 22) (5 33) (#t 44))))
(println (assoc #f '((1 11) (2 22) (3 33) (#f 44))))
(println (assoc #f '((1 11) (2 22) (3 33) (4 44))))

;#f
;(2 22)
;#f
;#f
;#f
;((2) 1)
;#f
;(bo #t)
;(#t 44)
;(#f 44)
;#f
