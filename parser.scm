;; fichier contenant les fonctions pour le parsing

(define abbrev
  '(( #\')
    ( #\`)
    ( #\,)
     ))

(define (read port)
  (let ((c (peek-char-non-whitespace port)))
    (cond ((eof-object? c)
           c)
          ((char=? c #\()   ;; list
           (read-char port) ;; skip "("
           (read-list port))
	  ((char=? c #\")   ;; string
           (read-string port))
	  ((char=? c #\;)   ;; comment
	   (read-line port) ;; skip to next line
	   (read port))
	  ((assoc c abbrev)
	   (read-abbrev port))
	  ((char=? c #\#)
	   (read-char port)
           (read-hashtag port))
          (else
           (read-char port) ;; skip first char
           (let ((s (list->string (cons c (read-symbol port)))))
             (or (string->number s)
                 (string->symbol s)))))))

;;support des abbrevation
(define (read-abbrev port)
  (let ((c (peek-char port)))
    (cond ((char=? c #\') ;;quote on le remplace par quote
	   (read-char port)
	   (list 'quote (read port)))
	  ((char=? c #\`)
	   (error "quasiquote not supported yet")) ;;quasiquote
 	  ((char=? c #\,)
	    (error ", not supported yet")) ;; , et , @
	  )))

;; (define (read-list port)
;;   (let ((c (peek-char-non-whitespace port)))
;;     (cond ((char=? c #\))
;;            (read-char port) ;; skip ")"
;;            '())
;;           ((char=? c #\.)
;;            )
;;           (else
;;            (cons (read port) (read-list port))))))

(define (read-list port)
  (let ((c (peek-char-non-whitespace port)))
    (cond ((char=? c #\))
           (read-char port) ;; consume ")"
           '())
          (else
           (let ((datum (read port)))
             (if (eq? datum (string->symbol "."))
                 (error "Improperly placed dot!!!!!")
                 (cons datum (read-list-mid port))))))))

(define (read-list-mid port)
  (let ((c (peek-char-non-whitespace port)))
    (cond ((char=? c #\))
           (read-char port) ;; consume ")"
           '())
          (else
           (let ((datum (read port)))
             (if (eq? datum (string->symbol "."))
                 (read-list-end port)
                 (cons datum (read-list-mid port))))))))


(define (read-list-end port)
  (let ((c (peek-char-non-whitespace port)))
    (if (char=? c #\))
        (error "Datum expected")
        (let* ((datum (read port))
               (c (peek-char-non-whitespace port)))
          (cond ((eq? datum (string->symbol "."))
                 (error "Datum expected"))
                ((char=? c #\))
                 datum)
                (else
                 (error "End of list expected")))))))

(define (read-string port)
  (read-char port) ;; skip opening quote
  (list->string (read-string2 port)))

(define (read-string2 port)
  (let ((c (peek-char port)))
    (cond ((eof-object? c)
           (error "EOF reached, quote expected"))
          ((char=? c #\")
           (read-char port) ;; skip closing quote
           '())
          ((char=? c #\\)
           (read-char port) ;; consume "\"
           (let ((esc-c (peek-char port)))
             (if (or (char=? esc-c #\")
                     (char=? esc-c #\\))
                 (cons (read-char port) (read-string2 port))
                 (cons c (read-string2 port)))))
          (else
           (read-char port) ;; consume c
           (cons c (read-string2 port))))))

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
	  (cons c (read-symbol port))))))

;; gestion du #
(define (read-hashtag port)
  (let ((c (peek-char port)))
    (cond ((char=? c #\t)
           (read-char port)
           '#t)
	  ((char=? c #\f)
           (read-char port)
           '#f)
	  ((char=? c #\\)
           (read-char port)
           (read-char port))
	  (else
	   (error "expected #f #t or #\\ character")))))

(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (or (eof-object? c)
            (char>? c #\space))
        c
        (begin
          (read-char port)
	  (peek-char-non-whitespace port)))))

;; (trace read-string)
;; (trace read)
;; (trace peek-char-non-whitespace)
;; (trace read-symbol)
;; (trace read-list)
;; (trace read-list-mid)
;; (trace read-list-end)

