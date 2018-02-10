;; fichier pour le parsage

;; token
(define (read port)
  (let ((c (peek-char-non-whitespace port)))
    (cond ((eof-object? c)
           c)
          ((char=? c #\()        ;; list
           (read-char port)      ;; skip "("
           (read-list port))
	  ((char=? c #\")        ;; string
           (read-string port))
	  ((char=? c #\;)        ;; comment
	   (read-line port)      ;; skip to next line
	   (read port))
	  ;; ((char=? c #\') ;; quote
	  ;;  (error "le quote n'est pas encore supporté"))
	  ;; ((char=? c #\#) ;; #
	  ;;  (read-char port)
	  ;;  (read-hashtag port))
          (else
           (read-char port) ;; skip first char
           (let ((s (list->string (cons c (read-symbol port)))))
             (or (string->number s)
                 (string->symbol s)))))))

(define (read-list port)
  (let ((c (peek-char-non-whitespace port)))
    (cond ((char=? c #\))
           (read-char port) ;; skip ")"
           '())
          ((char=? c #\")
           (read-string port))
          ((char=? c #\;)
           (read-line port) ;; skip comment
           (read-list port))
          (else
           (let ((first (read port)))
             (let ((rest (read-list port)))
               (cons first rest)))))))

(define (read-string port)
  (read-char port) ;; skip opening quote
  (println (list->string (read-string2 port))))

(define (read-string2 port)
  (let ((c (peek-char port)))
    (cond ((eof-object? c)
           (error "EOF reached, quote expected"))
          ((char=? c #\")
           (read-char port) ;; skip closing quote
           '())
          ((char=? c #\\)
           (read-char port) ;; advance
           (let ((esc-c (peek-char port)))
             (if (or (char=? esc-c #\")
                     (char=? esc-c #\\))
                 (cons (read-char port) (read-string2 port))
                 (cons c (read-string2 port)))))
          (else
           (read-char port) ;; advance
           (cons c (read-string2 port))))))
  
;; gestion du #
(define (read-hashtag port)
  (let ((c (peek-char port)))
    (cond ((char=? c #\t)
	   (begin
	     (read-char port)
	     (string->symbol "#t")))
	  ((char=? c #\f)
	   (begin
	     (read-char port)
	     (string->symbol "#f")))
	  ((char=? c #\\)
	   (begin
	     (read-char port)
	     (read-char port)))
	  (else
	   (error "expected #f #t or #\\ character")))))

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

;; get a non whitespace
(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (or (eof-object? c)
            (char>? c #\space))
        c
        (begin
          (read-char port)
	  (peek-char-non-whitespace port)))))

;; (trace read-string-element)
;; (trace read-string)
;; (trace read)
;; (trace peek-char-non-whitespace)
;; (trace read-symbol)
;; (trace read-list)
