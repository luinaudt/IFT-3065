;;(println println)
(let loop2 ((y 1))
;;  (println loop2)
  (if (< 15 y)
      (println "fin")
      (begin
	(println (let somme ((y (let loop ((x  100000))
				  (if (= x 0)
				      (begin
	;;				(println loop)
					'())
				      (cons x (loop (- x 1)))))))
		   (if (null? y)
		       0
		       (+ (car y) (somme (cdr y))))))
	(loop2 (+ y 1)))))

;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;5000050000
;fin
