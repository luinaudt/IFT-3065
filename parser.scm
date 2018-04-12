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

; (define (read-list port)
;   (let ((c (peek-char-non-whitespace port)))
;     (cond ((char=? c #\))
;            (read-char port) ;; skip ")"
;            '())
;           ((char=? c #\.)
;            )
;           (else
;            (cons (read port) (read-list port))))))

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
		 (read-char port)
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
	  ((char=? c #\!)
	   (read-char port)
	   (read-special-value port))
	  ((char=? c #\\)
           (read-char port)
           (read-character port))
	  (else
	   (error "expected #f #t or #\\ character")))))

(define (read-character port)
  (let* ((c (read-char port))
	 (n (peek-char port)))
    (cond ((or (eof-object? n)
	       (char=? n #\()
	       (char=? n #\')
	       (char=? n #\))
	       (char<=? n #\space))
	   c)
	  ((char=? c #\s)
	   (if (read-char-space port)
	       #\space
	       (error "not valid character")))
	  ((char=? c #\n)
	   (if (read-char-newline port)
	       #\newline
	       (error "not valid character")))
	  (else
	   (error "not valid character")))))

(define (read-special-value port)
  (cond ((char=? (peek-char port) #\v)
	 (if (read-void-value port)
	     #!void
	     (error "not valid special character")))
	 
	((char=? (peek-char port) #\u)
	 (if (read-unbound-value port)
	     #!unbound
	     (error "not valid special character")))
	(else
	 (error "not valid special character"))))
  
(define (read-void-value port)
  (if (char=? #\v (read-char port))
      (if (char=? #\o (read-char port))
	  (if (char=? #\i (read-char port))
	      (if (char=? #\d (read-char port))
		  (or (eof-object? (peek-char port))
		      (char=? #\( (peek-char port))
		      (char=? #\' (peek-char port))
		      (char=? #\) (peek-char port))
		      (char<=? (peek-char port) #\space))
		  #f)
	      #f)
	  #f)
      #f))

(define (read-unbound-value port)
  (if (char=? #\u (read-char port))
      (if (char=? #\n (read-char port))
	  (if (char=? #\b (read-char port))
	      (if (char=? #\o (read-char port))
		  (if (char=? #\u (read-char port))
		      (if (char=? #\n (read-char port))
			  (if (char=? #\d (read-char port))
			      (or (eof-object? (peek-char port))
				  (char=? #\( (peek-char port))
				  (char=? #\' (peek-char port))
				  (char=? #\) (peek-char port))
				  (char<=? (peek-char port) #\space))
			      #f)
			  #f)
		      #f)
		  #f)
	      #f)
	  #f)
      #f))
	

(define (read-char-space port)
  (if (char=? #\p (read-char port))
      (if (char=? #\a (read-char port))
	  (if (char=? #\c (read-char port))
	      (if (char=? #\e (read-char port))
		  (or (eof-object? (peek-char port))
		      (char=? #\( (peek-char port))
		      (char=? #\' (peek-char port))
		      (char=? #\) (peek-char port))
		      (char<=? (peek-char port) #\space))
		  #f)
	      #f)
	  #f)
      #f))
(define (read-char-newline port)
  (if (char=? #\e (read-char port))
      (if (char=? #\w (read-char port))
	  (if (char=? #\l (read-char port))
	      (if (char=? #\i (read-char port))
		  (if (char=? #\n (read-char port))
		      (if (char=? #\e (read-char port))
			  (or (eof-object? (peek-char port))
			      (char=? #\( (peek-char port))
			      (char=? #\' (peek-char port))
			      (char=? #\) (peek-char port))
			      (char<=? (peek-char port) #\space))
			  #f)
		      #f)
		  #f)
	      #f)
	  #f)
      #f))

(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (or (eof-object? c)
            (char>? c #\space))
        c
        (begin
          (read-char port)
	  (peek-char-non-whitespace port)))))

; (trace read-string)
;; (trace read)
;; (trace peek-char-non-whitespace)
; (trace read-symbol)
; (trace read-list)
; (trace read-list-mid)
; (trace read-list-end)

