;fichier pour le parsage

;get a non whitesace
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

(define (read-comment port)
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
		))))
	    (read-char port)
	)
    )
  
;gestion du #
(define (read-hashtag port)
  (let ((c (peek-char port)))
    (cond ((char=? c #\t)
	   (begin
	     (read-char port)
	     (string->symbol "#t")
	     )
	   )
	  ((char=? c #\f)
	   (begin
	     (read-char port)
	     (string->symbol "#f")
	     )
	   )
	  ((char=? c #\\)
	   (begin
	     (read-char port)
	     (read-char port)
	     )
	   )
	  (else
	   (error "expected #f #t or #\\ character")
	   )
	  )
    )
  )

;token
(define (read port)
  (let ((c (peek-char-non-whitespace port)))
    (cond ((eof-object? c)
           c)
          ((char=? c #\()
           (read-char port) ;; skip "("
           (read-list port))
	  ((char=? c #\;)   ;; commentaire
	   (read-comment port)
	   (read port)
	   )
	  ((char=? c #\")
	   (read-char port) ;; skip "
	   (list->string (read-string port))
	   )
	  ((char=? c #\') ; quote
	   (error "le quote n'est pas encore supporté")
	   )
	  ((char=? c #\#) ;#
	   (begin
	     (read-char port)
	     (read-hashtag port)
	     )
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
	    (char=? c #\')
            (char=? c #\))
            (char<=? c #\space))
	'()
	(begin
	  (read-char port)
	  (cons c (read-symbol port)))
	))
  )

;(trace read-string-element)
;(trace read-string)
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
