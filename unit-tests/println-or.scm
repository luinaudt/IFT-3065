(println (or (< 5 6)
	     #f))
(println (or #f))
(println (or #t))
(println (or (< 6 5)
	     (< 9 8)))
;#t
;#f
;#t
;#f
