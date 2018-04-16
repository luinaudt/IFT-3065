(let loop2 ((y 1))
  (if (< 15 y)
      (println "fin")
      (begin
	(println (let somme ((y (make-vector 100000 98)) (pos 0))
		   (if (= pos (vector-length y))
		       0
		       (+ (vector-ref y pos) (somme y (+ pos 1))))))
	(loop2 (+ y 1)))))


;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;9800000
;fin
