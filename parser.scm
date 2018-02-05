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
  (let ((c (read-char port)))
    (if (not (char=? c #\newline))
	(read-line port))
    '()
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
