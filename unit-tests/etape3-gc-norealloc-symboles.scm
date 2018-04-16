(define t 0)
(let loop2 ((y 1))
  (if (< 1500000 y)
      (println "fin")
      (begin
	(set! t 'untexterandomquejevaisafficherpleindefois)
	(loop2 (+ y 1)))))

;fin
