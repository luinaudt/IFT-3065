;; fichier contenant le fonction pour le parsage

;; read a datum
(define (read port)
  
  )

;;simple-datum
(define (read-simp-datum port)
  )

;;symbol
(define (read-symbol port)
  )

;;compound datum
(define (read-compound-datum port)
  )

;;list
(define (read-list port)
  )

;; abbrevation
(define (read-abbrevation port)
  )

;;abbrevation prefix
(define (read-abbrev-prefix port)
  )




(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (or (eof-object? c)
            (char>? c #\space))
        c
        (begin
          (read-char port)
	  (peek-char-non-whitespace port)))))
