(println (and (< 5 6)
	     #f))
(println (and #f))
(println (and #t))
(println (and (< 5 6)
	      (<  8 9)))
;#f
;#f
;#t
;#t
