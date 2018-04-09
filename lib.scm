;;; File: lib.scm

;;;============================================================================

;; Primitive functions

(define println  (lambda (x)   ($println x)))
(define +        (lambda (x y) ($+ x y)))
(define -        (lambda (x y) ($- x y)))
(define *        (lambda (x y) ($* x y)))
(define quotient (lambda (x y) ($quotient x y)))
(define modulo   (lambda (x y) ($modulo x y)))
(define =        (lambda (x y) ($= x y)))
(define <        (lambda (x y) ($< x y)))
(define remainder (lambda (x y) ($remainder x y)))
(define number?  (lambda (x)   ($number? x)))

;(define read-char     (lambda ()  ($read-char)))
;(define write-char    (lambda (x) ($write-char x)))
;(define integer->char (lambda (x) ($integer->char x)))
;(define char->integer (lambda (x) ($char->integer x)))
;(define char?         (lambda (x) ($char? x)))

;(define make-string   (lambda (x)   ($make-string x)))
;(define string-ref    (lambda (x y) ($string-ref x y)))
;(define string-set!   (lambda (x y) ($string-set! x y)))
;(define string-length (lambda (x y) ($string-length x y)))
;(define string?       (lambda (x)   ($string? x)))

;(define cons     (lambda (x y) ($cons x y)))
;(define car      (lambda (x)   ($car x)))
;(define cdr      (lambda (x)   ($cdr x)))
;(define set-car! (lambda (x y) ($set-car! x y)))
;(define set-cdr! (lambda (x y) ($set-cdr! x y)))
;(define pair?    (lambda (x)   ($pair? x)))

;(define procedure? (lambda (x)   ($procedure? x)))
;(define eq?        (lambda (x y) ($eq? x y)))

;;;============================================================================

;; Fonctions prédéfinies

;; (define string=?
;;   (lambda (x y)
;;     (if (and ($string? x)
;;              ($string? y)
;;              ($= ($string-length x) ($string-length y)))
;;         ;; compare here
;;         #f)))

;; (define eqv?
;;   (lambda (x y)
;;     (if (and ($number? x) ($number? y))
;;         ($= x y)
;;         ($eq? x y))))

;; (define equal?
;;   (lambda (x y)
;;     (cond (($pair? x)
;;            (and ($pair? y)
;;                 (equal? ($car x) ($car y))
;;                 (equal? ($cdr x) ($cdr y))))
;;           ;;(($string? x)
;;           ;; (and ($string? y)
;;           ;;      (string=? x y)))
;;           (else
;;            (eqv? x y)))))

;; (define not
;;   (lambda (x)
;;     (if ($eq? x #f) #t #f)))

;; (define boolean?
;;   (lambda (x)
;;     (or ($eq? x #t) ($eq? x #f))))

;; ;; (define null?
;; ;;   (lambda (x)
;; ;;     ($eq? x '())))

;; ;; (define member
;; ;;   (lambda (x lst)
;; ;;     (cond ((null? lst)
;; ;;            #f)
;; ;;           ((equal? x ($car lst))
;; ;;            lst)
;; ;;           (else
;; ;;            (member x ($cdr lst))))))

;; ;; (define assoc
;; ;;   (lambda (k lst)
;; ;;     (cond ((null? lst)
;; ;;            #f)
;; ;;           ((equal? k ($car ($car lst)))
;; ;;            ($car lst))
;; ;;           (else
;; ;;            (assoc k ($cdr lst))))))

;; ;; (define append
;; ;;   (lambda (x y)
;; ;;     (if (null? x)
;; ;;         y
;; ;;         ($cons ($car x)
;; ;;                (append ($cdr x) y)))))

;; ;; (define reverse
;; ;;   (lambda (x)
;; ;;     (let loop ((x x) (acc '()))
;; ;;       (if (null? x)
;; ;;           acc
;; ;;           (loop ($cdr x)
;; ;;                 ($cons ($car x) acc))))))

;; ;; (define length
;; ;;   (lambda (x)
;; ;;     (if (null? x)
;; ;;         0
;; ;;         ($+ 1 (length ($cdr x))))))

;; ;; (define map
;; ;;   (lambda (f lst)
;; ;;     (if (null? lst)
;; ;;         '()
;; ;;         ($cons (f ($car lst))
;; ;;                (map f ($cdr lst))))))

;; ;; (define char=?
;; ;;   (lambda (x y)
;; ;;     ($= ($char->integer x) ($char->integer y))))

;; ;; (define char<?
;; ;;   (lambda (x y)
;; ;;     ($< ($char->integer x) ($char->integer y))))

;; ;; (define string<?
;; ;;   (lambda (x y)
;; ;;     (if (and ($string? x)
;; ;;              ($string? y))
;; ;;         ;; compare here
;; ;;         #f)))

;; ;; (define read
;; ;;   (lambda ()
;; ;;     ;; TODO
;; ;;     ))

;; ;; (define write
;; ;;   (lambda (x)
;; ;;     ;; TODO
;; ;;     ))

;; ;;;============================================================================
