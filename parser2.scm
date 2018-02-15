;;nouvelle fonction de parsage


(define (delimiter? c)
  (or (char<? c #\!) ;;plus petit ou egal a \space
      (eof-object? c)
      (char=? #\()
      (char=? #\))
      (char=? #\")
      (char=? #\;)))

;;datum
(define (read port)
  
  )

;;simple datum
(define (read-simple port)
  )

;;read symbol
(define (read-symbol port)
  )

;;read compound
(define (read-compound port)
  )

;;read list
(define (read-list port)
  )

;;read abbreviation
(define (read-abbrev port)
  )

;;vector
(define (read-vector port)
  )


;;recupérer nouveau caractère
(define (peek-char-nws port)
  (let ((c (peek-char port)))
    (if (or (eof-object? c)
            (char>? c #\space))
        c
        (begin
          (read-char port)
          (peek-char-nws port)))))
 


