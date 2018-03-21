(println (if (let ((une_superbe_variable 154)
		   (x (* 5 6))
		   (y (quotient 15 6)))
	       (println x)
	       (println y)
	       (println une_superbe_variable)
	       (- x y)
	       )
	     #t
	     156))

(println (if (let ((une_superbe_variable 154)
		   (x (+ 8 9))
		   (y (quotient 15 154)))
	       (println x)
	       (println y)
	       (println une_superbe_variable)
	       (println (modulo x 154))
	       (- x y)
	       #t
	       )
	     156
	     256 
	     ))

;30
;2
;154
;#t
;17
;0
;154
;17
;156
