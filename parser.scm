;fichier pour le parsage

(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (or (eof-object? c)
            (char>? c #\space))
        (begin
	  c)
        (begin
          (read-char port)
	  (peek-char-non-whitespace port))
	)
    ))

(define (ignore-line port)
  (let ((c (peek-char port)))
    (if (not (char=? c #\newline))
	(begin
	  (read-char port) 
	  (read-line port))
	)
    '()
    )
  )

;lecture d'une chaîne de caractères
(define (read-string port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	(error "manque un \" ")
	(cond ((char=? c #\")
	       (begin
		 (read-char port)
		 '()
		 ))
	      (else
	       (begin
		 (read-string-element port)
		 (cons c (read-string port))))
	      )
	)
    )
  )
;get a string element
(define (read-string-element port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	(error "fin de lecture pour chaîne")
	(if (char=? c #\\)
	    (begin
	      (read-char port)
	      (let ((c (peek-char port)))
		(if (not (or (char=? c #\")
			     (char=? c #\\)))
		    (error "caractère échappé invalide #\\" c)
		    )
		))
	    (read-char port))
	)
    )
  )



(define (read port)
  (let ((c (peek-char-non-whitespace port)))
    (cond ((eof-object? c)
           c)
          ((char=? c #\()
           (read-char port) ;; skip "("
           (read-list port))
	  ((char=? c #\;)   ;; commentaire
	   (ignore-line port)
	   (read port)
	   )
	  ((char=? c #\")
	   (read-char port) ;; skip "
	   (list->string(read-string port))
	   )
          (else
           (read-char port) ;; skip first char
           (let ((s (list->string (cons c (read-symbol port)))))
             (or (string->number s)
                 (string->symbol s)))))))

(define (read-list port)
  (let ((c (peek-char-non-whitespace port)))
    (if (char=? c #\))
        (begin
          (read-char port) ;; skip ")"
          '())
        (let ((first (read port)))
          (let ((rest (read-list port)))
            (cons first rest))))))

(define (read-symbol port)
  (let ((c (peek-char port)))
    (if (or (eof-object? c)
	    (char=? c #\()
            (char=? c #\))
            (char<=? c #\space))
	'()
	(begin
	  (read-char port)
	  (cons c (read-symbol port)))
	))
  )

					;(trace read)
					;(trace peek-char-non-whitespace)
					;(trace read-symbol)
					;(trace read-list)

(define (parse-exprs port)
  (let ((x (read port)))
    (if (eof-object? x)
	'() ;fin de texte
	(cons x
	      (parse-exprs port))))
  )
