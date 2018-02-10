;; fichier pour la génération du code assembleur

(define dict
  (list (#f 1)
        (#t 9)
        ))

(define (lookup key dict)
  (let ((key-val (assoc key dict)))
    (if key-val
        (cadr key-val)
        #f)))


;; (define (list? lst)
;;   (or (null? lst)
;;       (list? (cdr lst))))

;; (define (pair? p)
;;   (and (not (null? p))
;;        (list? p)))

;; (define (number? n)
;;   (and (not (null? n))
;;        (not (pair? n))
;;        (= (modulo n 8) 0)))

;; (define (boolean? b)
;;   (and (not (null? b))
;;        (not (pair? b))
;;        (= (modulo b 8) 1)))

;; (define (char? c)
;;   (and (not (null? c))
;;        (not (pair? c))
;;        (= (modulo c 8) 2)))


;; on compile un bloc complet
(define (compile-expr expr)
  (if (pair? expr)
      (cons (analyse-expr expr)
            "pop %rax\n")
      (error "unknown expression" expr)))


;; on compile une fonction unique
(define (analyse-expr expr)
  (if (pair? expr)
      (cond ((equal? 'if (car expr))
	     (analyse-conditional (cdr expr)))
	    ((equal? 'set! (car expr))
	     (analyse-set! (cdr expr)))
	    ((equal? 'let (car expr))
	     (analyse-let (cdr expr)))
	    (else
	     (analyse-proc expr)))
      (analyse-operand expr)))


;; analyse du set!
(define (analyse-set! expr)
  (error "set! not supported yet"))


;; analyse du let
(define (analyse-let expr)
  (cond ((pair? (car expr))
	 (analyse-binding-let (car expr)))
	((null? (car expr))
	 (pp "reussi"))
	(else (error "illegal form for let expression " expr))))


;; analyse du premier paramètre de let
(define (analyse-binding-let expr)
  (if (null? expr)
      '()
      (if (or (pair? (car expr))
	      (= (length (car expr)) 2))
	  (cons (analyse-expr (car expr))
		(analyse-binding-let (cdr expr)))
	  (error "illegal form for let expression" expr))))


;; analyse d'une condition (if)
(define (analyse-conditional expr)
  (if (= (length expr) 3)
      (cons (cons (analyse-expr (car expr))
                  (list " pop %rax \n"
                        " cmp $9, %rax \n"
                        " jne label1if \n"))
            (cons (cons (analyse-expr (cadr expr))
                        (list "jmp fin \n"
                              "label1if:\n"))
                  (cons (analyse-expr (caddr expr))
                        "fin: \n")))
      (error "unvalid construct for if")))


;; fonction analyser les opérande d'une fonction
(define (analyse-operand expr)
  (if (null? expr)
      '()
      (if (pair? expr)
          (analyse-expr expr)
          (cond ((number? expr)
                 (list " mov $" expr ",%rax\n"
                       " mov $8, %rbx\n"
                       " mul %rbx\n"
                       " push %rax \n"))
                ((equal? expr (string->symbol "#f"))
                 (list " push $1 \n"))
                ((equal? expr (string->symbol "#t"))
                 (list " push $9 \n"))
                (else (error "parametre invalide" expr))))))


;; analyse procedure
(define (analyse-proc expr)
  (if (= (length expr) 2)
      (cons (analyse-operand (cadr expr))
	    (analyse-op (car expr)))
      (if (= (length expr) 3)
	  (cons  (cons (analyse-operand (cadr expr))
                       (analyse-operand (caddr expr)))
		 (analyse-op (car expr)))
	  (error "expression non valide" expr))))


(define op-table '((+         (" add  %rbx, %rax\n"
                               " push %rax\n"))
                   (-         (" sub  %rbx, %rax\n"
                               " push %rax\n"))
                   (*         (" sar  $3, %rbx\n"
			       " mul  %rbx\n"
                               " push %rax\n"))
                   (quotient  (" cqo\n"
                               " idiv %rbx\n"
			       " sal $3, %rax\n"
                               " push %rax\n"))
                   (modulo    (" cqo \n"
                               " idiv %rbx\n"
                               " push %rdx\n"))
                   (<         (" cmp  %rbx, %rax\n"
                               " mov  $1, %rax\n"
                               " mov  $9, %rbx\n"
                               " cmovs %rbx, %rax\n"
                               " push %rax\n"))
                   (=         (" cmp  %rbx, %rax\n"
                               " mov  $1, %rax\n"
                               " mov  $9, %rbx\n"
                               " cmovz %rbx, %rax\n"
                               " push %rax\n"))))


;; fonction pour analyser une opération (premier élément d'une parenthèse)
(define (analyse-op expr)
  (cond ((equal?  expr 'println)
         (list  " call print_ln\n"
                " push $10\n"
                " call putchar\n"
		" push $0\n"))
        ((lookup expr op-table)
         (list " pop %rbx\n"
               " pop %rax\n"
               (lookup expr op-table)))
        (else
         (error "unknown operation" expr))))

;; (trace compile-expr)
;; (trace analyse-op)
;; (trace analyse-operand)
;; (trace analyse-proc)
;; (trace analyse-expr)
;; (trace analyse-binding-let)
;; (trace analyse-let)

(define (compile-program exprs)
  (list " .text\n"
        " .globl _main\n"
        " .globl main\n"
        "_main:\n"
        "main:\n"
        (map compile-expr exprs)
        " mov $0, %rax\n"
        " ret\n"))
