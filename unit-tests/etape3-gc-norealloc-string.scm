(define t 0)
(let loop2 ((y 1))
  (if (< 150000 y)
      (println "fin string symbole")
      (begin
	(set! t '"untexterandomquejevaisafficherpleindefois")
	(loop2 (+ y 1)))))
(let loop2 ((y 1))
  (if (< 150000 y)
      (println "fin string")
      (begin
	(set! t "untexterandomquejevaisafficherpleindefois")
	(loop2 (+ y 1)))))


;fin string symbole
;fin string
