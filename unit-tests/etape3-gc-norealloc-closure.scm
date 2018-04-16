(define nit 100000)
(let loop2 ((y 1) (z 5) (u 8) (v 10))
  (if (< nit y)
      (println "fin")
      (begin
	(let somme ((y 5) (x 2))
		   (+ x (+ y (+ z (+ u v)))))
	(loop2 (+ y 1) z u (+ z u)))))
(let loop2 ((y 1) (z 5) (u 8) (v 10))
  (if (< nit y)
      (println "fin")
      (begin
	(let somme ((y 5) (x 2))
		   (+ x (+ y (+ z (+ u v)))))
	(loop2 (+ y 1) z u (+ z u)))))
(let loop2 ((y 1) (z 5) (u 8) (v 10))
  (if (< nit y)
      (println "fin")
      (begin
	(let somme ((y 5) (x 2))
		   (+ x (+ y (+ z (+ u v)))))
	(loop2 (+ y 1) z u (+ z u)))))
(let loop2 ((y 1) (z 5) (u 8) (v 10))
  (if (< nit y)
      (println "fin")
      (begin
	(let somme ((y 5) (x 2))
		   (+ x (+ y (+ z (+ u v)))))
	(loop2 (+ y 1) z u (+ z u)))))
(let loop2 ((y 1) (z 5) (u 8) (v 10))
  (if (< nit y)
      (println "fin")
      (begin
	(let somme ((y 5) (x 2))
		   (+ x (+ y (+ z (+ u v)))))
	(loop2 (+ y 1) z u (+ z u)))))

;fin
;fin
;fin
;fin
;fin
;fin

