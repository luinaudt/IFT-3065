;; Fichier "genconst.scm"

;; Ce fichier montre une façon de convertir une constante littérale
;; en une séquence de définitions de variables globales qui construisent
;; la constante à l'exécution.
;;
;; L'idée est de transformer un programme source tel que :
;;
;;   (write '(5 ("allo" #\x)))
;;
;; en
;;
;;   (define _1
;;     (let ((str ($make-string 4)))
;;       ($string-set! str 0 #\a)
;;       ($string-set! str 1 #\l)
;;       ($string-set! str 2 #\l)
;;       ($string-set! str 3 #\o)
;;       str))
;;   (define _2 ($cons #\x '()))
;;   (define _3 ($cons _1 _2))
;;   (define _4 ($cons _3 '()))
;;   (define _5 ($cons 5 _4))
;;   (write _5)

(define const-code '())
(define const-count 0)

(define (def-const construct)
  (set! const-count (+ const-count 1))
  (let ((var (string->symbol (string-append "_" (number->string const-count)))))
    (set! const-code (cons `(define ,var ,construct) const-code))
    var))

(define (gen-const obj)
  (begin
    (set! const-code '())
    (gen-const-helper obj)))

(define (gen-const-helper obj)

  (cond ((pair? obj)
         (def-const
           `($cons ,(gen-const-helper (car obj))
                   ,(gen-const-helper (cdr obj)))))

        ((string? obj)
         (def-const
           `(let ((str ($make-string ,(string-length obj))))
              ,@(map (lambda (i) `($string-set! str ,i ,(string-ref obj i)))
                     (iota (string-length obj)))
              str)))

        ((or (number? obj) (boolean? obj) (char? obj))
         obj)

        (else
         `(quote ,obj))))

(define (iota n)
  (let loop ((i (- n 1)) (lst '()))
    (if (>= i 0)
        (loop (- i 1) (cons i lst))
        lst)))

;; (pretty-print (gen-const '(5 ("allo" #\x))))

;; (for-each
;;  pretty-print
;;  (reverse const-code))
