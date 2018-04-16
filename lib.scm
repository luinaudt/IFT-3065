;;; File: lib.scm

;;;============================================================================

;; Primitive functions

(define println   (lambda (k x)   (k ($println x))))  ;; glob_0
(define +         (lambda (k x y) (k ($+ x y))))
(define -         (lambda (k x y) (k ($- x y))))
(define *         (lambda (k x y) (k ($* x y))))
(define quotient  (lambda (k x y) (k ($quotient x y))))
(define remainder (lambda (k x y) (k ($remainder x y))))
(define modulo    (lambda (k x y) (k ($modulo x y))))
(define =         (lambda (k x y) (k ($= x y))))
(define <         (lambda (k x y) (k ($< x y))))
(define number?   (lambda (k x)   (k ($number? x))))

(define read-char     (lambda (k)  (k ($read-char))))  ;; glob_10
(define write-char    (lambda (k x) (k ($write-char x))))
(define integer->char (lambda (k x) (k ($integer->char x))))
(define char->integer (lambda (k x) (k ($char->integer x))))
(define char?         (lambda (k x) (k ($char? x))))

(define make-string   (lambda (k x)     (k ($make-string x))))  ;; glob_15
(define string-ref    (lambda (k x y)   (k ($string-ref x y))))
(define string-set!   (lambda (k x y z) (k ($string-set! x y z))))
(define string-length (lambda (k x)     (k ($string-length x))))
(define string?       (lambda (k x)     (k ($string? x))))

(define cons     (lambda (k x y) (k ($cons x y))))  ;; glob_20
(define car      (lambda (k x)   (k ($car x))))
(define cdr      (lambda (k x)   (k ($cdr x))))
(define set-car! (lambda (k x y) (k ($set-car! x y))))
(define set-cdr! (lambda (k x y) (k ($set-cdr! x y))))
(define pair?    (lambda (k x)   (k ($pair? x))))

(define procedure? (lambda (k x)   (k ($procedure? x))))
(define eq?        (lambda (k x y) (k ($eq? x y))))

(define make-vector (lambda (k len val) (k ($make-vector len val))))
(define vector-length (lambda (k x) (k ($vector-length x))))
(define	vector-ref (lambda (k vec pos) (k ($vector-ref vec pos))))
(define	vector-set! (lambda (k vec pos obj) (k ($vector-set! vec pos obj))))
(define	vector? (lambda (k x) (k ($vector? x))))

(define symbol? (lambda (k x) (k ($symbol? x))))
(define symbol->string (lambda (k x) (k ($symbol->string x))))
;;;============================================================================

;; Fonctions prédéfinies

(define eqv?
  (lambda (k x y)
    (k (if (and ($number? x) ($number? y))
           ($= x y)
           ($eq? x y)))))

(define equal?
  (lambda (k x y)
    (k (cond (($pair? x)
              (and ($pair? y)
                   (equal? ($car x) ($car y))
                   (equal? ($cdr x) ($cdr y))))
             (($string? x)
              (and ($string? y)
                   (let ((len ($string-length x)))
                     (and ($= len ($string-length y))
                          (let loop ((i 0))
                            (or ($= i len)
                                (and ($eq? ($string-ref x i) ($string-ref y i))
                                     (loop ($+ i 1)))))))))
             (else
              (if (and ($number? x) ($number? y))
                  ($= x y)
                  ($eq? x y)))))))

(define not
  (lambda (k x)
    (k ($eq? x #f))))

(define boolean?
  (lambda (k x)
    (k (or ($eq? x #t) ($eq? x #f)))))

(define null?
  (lambda (k x)
    (k ($eq? x '()))))

(define list?
  (lambda (k x)
    (k (or ($eq? x '())
           (and ($pair? x)
                (list? ($cdr x)))))))

(define length
  (lambda (k x)
    (k (if ($eq? x '())
           0
           ($+ 1 (length ($cdr x)))))))

(define map
  (lambda (k f lst)
    (k (if ($eq? lst '())
           '()
           (cons (f ($car lst))
                 (map f ($cdr lst)))))))

(define member
  (lambda (k x lst)
    (k (cond (($eq? lst '())
              #f)
             ((let equal? ((x x) (y ($car lst)))
                (cond (($pair? x)
                       (and ($pair? y)
                            (equal? ($car x) ($car y))
                            (equal? ($cdr x) ($cdr y))))
                      (($string? x)
                       (and ($string? y)
                            (let ((len ($string-length x)))
                              (and ($= len ($string-length y))
                                   (let loop ((i 0))
                                     (or ($= i len)
                                         (and ($eq? ($string-ref x i) ($string-ref y i))
                                              (loop ($+ i 1)))))))))
                      (else
                       (if (and ($number? x) ($number? y))
                           ($= x y)
                           ($eq? x y)))))
              lst)
             (else
              (member x ($cdr lst)))))))

(define assoc
  (lambda (k x lst)
    (k (cond (($eq? lst '())
              #f)
             ((let equal? ((x x) (y ($car ($car lst))))
                (cond (($pair? x)
                       (and ($pair? y)
                            (equal? ($car x) ($car y))
                            (equal? ($cdr x) ($cdr y))))
                      (($string? x)
                       (and ($string? y)
                            (let ((len ($string-length x)))
                              (and ($= len ($string-length y))
                                   (let loop ((i 0))
                                     (or ($= i len)
                                         (and ($eq? ($string-ref x i) ($string-ref y i))
                                              (loop ($+ i 1)))))))))
                      (else
                       (if (and ($number? x) ($number? y))
                           ($= x y)
                           ($eq? x y)))))
              ($car lst))
             (else
              (assoc x ($cdr lst)))))))

(define char=?
  (lambda (k x y)
    (k ($= ($char->integer x) ($char->integer y)))))

(define char<?
  (lambda (k x y)
    (k ($< ($char->integer x) ($char->integer y)))))

(define string=?
  (lambda (k x y)
    (k (let ((len ($string-length x)))
         (and ($= len ($string-length y))
              (let loop ((i 0))
                (or ($= i len)
                    (and ($eq? ($string-ref x i) ($string-ref y i))
                         (loop ($+ i 1))))))))))

(define string<?
  (lambda (k x y)
    (k (let* ((len-x ($string-length x))
              (len-y ($string-length y))
              (len (if ($< len-x len-y) len-x len-y)))
         (let loop ((i 0))
           (if ($= i len)
               ($< len-x len-y)
               (let ((cx ($char->integer ($string-ref x i)))
                     (cy ($char->integer ($string-ref y i))))
                 (or ($< cx cy)
                     (and ($= cx cy)
                          (loop ($+ i 1)))))))))))

(define append
  (lambda (k x y)
    (k (if ($eq? x '())
           y
           ($cons ($car x)
                  (append ($cdr x) y))))))

(define reverse
  (lambda (k x)
    (k (let loop ((x x) (acc '()))
         (if ($eq? x '())
             acc
             (loop ($cdr x)
                   ($cons ($car x) acc)))))))

(define read
  (lambda (k)
                                        ;(let get-vals
    (k (let read-chaine ((x ($read-char)))
         (if ($eq? x 0)
             '()
             ($cons x (read-chaine ($read-char)))))
       )))

(define write
  (lambda (k x)
    (k (cond (($string? x)
              (begin
                ($write-char #\")
                (let write-string ((x x) (pos 0))
                  (if ($< pos ($string-length x))
                      (let ((c ($string-ref x pos)))
                        (cond (($eq? c #\newline)
                               (begin ($write-char #\\)
                                      ($write-char #\n)))
                              (($eq? c #\")
                               (begin ($write-char #\\)
                                      ($write-char c)))
                              (($eq? c #\\)
                               (begin ($write-char #\\)
                                      ($write-char c)))
                              (else
                               ($write-char c)))
                        (write-string x ($+ 1 pos)))))
                ($write-char #\")))
             (($symbol? x)
              (let write-string ((x ($symbol->string x)) (pos 0))
                (if ($< pos ($string-length x))
                    (begin
                      ($write-char ($string-ref x pos))
                      (write-string x ($+ 1 pos))))))
             (($char? x)
              (begin
                ($write-char #\#)
                ($write-char #\\)
                (cond (($eq? x #\newline)
                       (begin
                         ($write-char #\n) ($write-char #\e) ($write-char #\w)
                         ($write-char #\l) ($write-char #\i) 
                         ($write-char #\n) ($write-char #\e)))
                      (($eq? x #\space)
                       (begin
                         ($write-char #\s) ($write-char #\p) ($write-char #\a)
                         ($write-char #\c) ($write-char #\e)))
                      (else
                       ($write-char x)))))
             (($number? x)
              (cond (($< x 0)
                     (begin
                       ($write-char #\-)
                       (write ($- 0 x))))
                    (($< x 10)
                     ($write-char ($integer->char ($+ 48 x))))
                    (else
                     (begin
                       (write ($quotient x 10))
                       (write ($modulo x 10))))))
             (($eq? x #t)
              (begin
                ($write-char #\#)
                ($write-char #\t)))
             (($eq? x #f)
              (begin
                ($write-char #\#)
                ($write-char #\f)))
             (($eq? x '())
              (begin
                ($write-char #\()
                ($write-char #\))))
             (($vector? x)
              (begin
                ($write-char #\#)
                ($write-char #\()
                (let write-vec ((x x) (pos 0))
                  (if ($< pos ($vector-length x))
                      (begin
                        (write ($vector-ref x pos))
                        (if ($< pos (- ($vector-length x) 1))
                            ($write-char #\space))
                        (write-vec x (+ 1 pos)))))
                ($write-char #\))))
             (($pair? x)
              (begin
                ($write-char #\()
                (write ($car x))
                (let write-pair ((cdr-x ($cdr x)))
                  (cond (($eq? cdr-x '())
                         ($write-char #\) ))
                        (($pair? cdr-x)
                         (begin
                           ($write-char #\space )
                           (write ($car cdr-x))
                           (write-pair ($cdr cdr-x))))
                        (else
                         (begin
                           ($write-char #\space )
                           ($write-char #\.)
                           ($write-char #\space )
                           (write cdr-x)
                           ($write-char #\))))))))
             (($procedure? x)
              (begin
                (let write-string ((x "#<procedure ") (pos 0))
                  (if ($< pos ($string-length x))
                      (begin
                        ($write-char ($string-ref x pos))
                        (write-string x ($+ 1 pos)))))
                ($write-char #\>)))
             (else
              ($write-char #\#))
             ))))

;;;============================================================================
