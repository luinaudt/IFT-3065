;;fichier pour la

(define (parse-exprs port)
  (let ((x (read port)))
    (if (eof-object? x)
	'() ;fin de texte
	(cons x
	      (parse-exprs port))))
  )
