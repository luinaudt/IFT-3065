;; fichier pour la génération du code assembleur



(define op-table
  '((+         (" add  %rbx, %rax\n"
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
    (modulo    (" sar $3, %rax\n"
		" sar $3, %rbx\n"
		" cqo\n"
		" idiv %rbx\n"
		" mov %rbx, %r8\n"
		" mov %rdx, %r9\n"
		" shr $63, %r8\n"
		" shr $63, %r9\n"
		" cmp %r8, %r9\n"
		" mov $0, %r8\n"
		" cmovne %rbx, %r8\n"
		" add %r8, %rdx\n"
		" sal $3, %rdx\n"
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
                " push %rax\n"))
    ))

;; #f et #t n'ont pas leur place dans l'environnement, on devra implanter
;; des macros éventuellement pour les substituer par leur valeur
(define env
  '(
    ))
(define stack 0)


(define (lookup key dict)
  (let ((key-val (assoc key dict)))
    (if key-val
        (cadr key-val)
        #f)))

(define gen list)

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


(define (compile-bloc exprs)
  (if (pair? exprs)
      (cons (compile-expr exprs)
            " pop %rax\n\n\n")
      (error "unknown expression" exprs)))

(define (compile-expr expr)
  (cond ((or (number? expr)
	     (equal? expr '#f)
	     (equal? expr '#t))
         (gen-literal expr))
        ((list? expr)
         (if (null? expr)
             (gen-list expr)
             (let ((first (car expr)) (rest (cdr expr)))
               (cond ((assoc first prims)
		      ((lookup first prims) rest))
                     ((assoc first env)
                      ((lookup first env) rest))
                     ((or (assoc first op-table)
                          (equal? first 'println))
                      (analyse-proc expr))
                     (else
                      (gen-list expr))))))
        ((pair? expr)
         (gen-pair expr))
        ((assoc expr env)
         (gen-var (lookup expr env)))
        (else
         (error "unknown expression" expr))))

(define (gen-var n)
  (gen " mov -" (number->string n) "(%rbp), %rax\n"
       " push %rax\n")
  )

(define (gen-literal n)
  (cond ((number? n)
	 (gen " mov $" (number->string (* 8 n)) ", %rax\n"
	      " push %rax\n"))
	((equal? n '#f)
	 (gen " mov $1, %rax\n"
	      " push %rax\n"))
	 ((equal? n '#t)
	  (gen " mov $9, %rax\n"
	       " push %rax\n"))))

;;liste de littéraux
(define (gen-list lst)
  (error "gen-list not yet implemented"))

(define (gen-pair p)
  (error "gen-pair not yet implemented"))

;;comilation d'un if
(define (compile-if exprs)
  (cond ((= (length exprs) 3)
         (let ((jmp-false (symbol->string (gensym)))
               (jmp-endif (symbol->string (gensym))))
           (gen (compile-expr (car exprs))
		" pop %rax\n"
		" cmp $1, %rax\n"
                " je " jmp-false "\n"
                (compile-expr (cadr exprs))
                " jmp " jmp-endif "\n"
                jmp-false ":\n"
                (compile-expr (caddr exprs))
                jmp-endif ":\n")))
        ((= (length exprs) 2)
         (let ((jmp-endif (symbol->string (gensym))))
           (gen (compile-expr (car exprs))
                " cmp $9, 4(%rsp)\n"
                " jne " jmp-endif "\n"
                (compile-expr (cadr exprs))
                jmp-endif ":\n")))
        (else
         (error "invalid construct: if"))))

;;compilation pour le let
(define (compile-let exprs)
  (if (< (length exprs) 2)
      (error "invalid construct: let")
      (let ((old-env env) (old-stack stack) (ret '()))
        ;; ajoute à l'environnement les nouveaux bindings
;;        (compile-bindings (car exprs) '())
	
        ;; compile le corps du let
	;; on sauvegarde l'environnement en mettant le rsp et en gardant rbp
	;; TODO: ajouter la sauvegarde de l'environnement avec des registres ?
	(begin (set! ret (cons (cons (if (= 0 old-stack)
					 " push %rbp \n mov %rsp, %rbp\n"
					 "")
				     (compile-bindings (car exprs) '()))
			       (cons (map compile-expr (cdr exprs))
				     (if (= 0 old-stack)
					 " mov %rbp, %rsp \n pop %rbp\n push %rax\n"
					 ""))))
	       ;; retourne l'environnement à son état original
	       (set! env old-env)
	       (set! stack old-stack)
	       ret )
	)))



;; Pour le let on doit avoir une stack au demarrage.
;; cela implique un partage de l'environnement. Doit on faire une structure env pour une meilleur evolution

;; traitement des bindings du let 
(define (compile-bindings bindings let-env)
  (if (not (null? bindings))
      (let ((first (car bindings)) (rest (cdr bindings)))
        (cond ((assoc (car first) let-env)
               (error "duplicate variable in let bindings"))
              ((or (pair? first) (= (length first) 2))
               (let ((new-bind (list (car first)
                                     (+ 8 stack))))
		 (set! stack (+ 8 stack))
		 (set! env (if (null? env) (cons new-bind '()) (cons new-bind env)))
                 (cons (compile-expr (cadr first))
		       (compile-bindings rest
                                   (cons new-bind let-env)))
                 ;; on modifie l'environnement à la fin car un binding
                 ;; ne doit pas influencer les autres bindings
		 ;;(set! env (cons new-bind env))))
		 ))
              (else
               (error "invalid binding construct: let"))))
        '() ))

;;assignation des variables
(define (compile-set! exprs)
  (error "set! not supported yet"))



;; ;; on compile une fonction unique
;; (define (analyse-expr expr)
;;   (if (pair? expr)
;;       (cond ((equal? 'if (car expr))
;; 	     (analyse-conditional (cdr expr)))
;; 	    ((equal? 'set! (car expr))
;; 	     (analyse-set! (cdr expr)))
;; 	    ((equal? 'let (car expr))
;; 	     (analyse-let (cdr expr)))
;; 	    (else
;; 	     (analyse-proc expr)))
;;       (analyse-operand expr)))


;; ;; analyse d'une condition (if)
;; (define (analyse-conditional expr)
;;   (if (= (length expr) 3)
;;       (cons (cons (analyse-expr (car expr))
;;                   (list " pop %rax \n"
;;                         " cmp $9, %rax \n"
;;                         " jne label1if \n"))
;;             (cons (cons (analyse-expr (cadr expr))
;;                         (list "jmp fin \n"
;;                               "label1if:\n"))
;;                   (cons (analyse-expr (caddr expr))
;;                         "fin: \n")))
;;       (error "unvalid construct for if")))

;; ;; analyse du let
;; (define (analyse-let expr)
;;   (cond ((pair? (car expr))
;; 	 (analyse-binding-let (car expr)))
;; 	((null? (car expr))
;; 	 (pp "reussi"))
;; 	(else (error "illegal form for let expression " expr))))

;; ;; analyse du premier paramètre de let
;; (define (analyse-binding-let expr)
;;   (if (null? expr)
;;       '()
;;       (if (or (pair? (car expr))
;; 	      (= (length (car expr)) 2))
;; 	  (cons (analyse-expr (car expr))
;; 		(analyse-binding-let (cdr expr)))
;; 	  (error "illegal form for let expression" expr))))

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

;; fonction analyser les opérande d'une fonction
(define (analyse-operand expr)
  (if (null? expr)
      '()
      (if (pair? expr)
          (compile-expr expr)
          (cond ((number? expr)
                 (list " mov $" expr ",%rax\n"
                       " sal $3, %rax\n"
                       " push %rax \n"))
                ((equal? expr '#f)
                 (list " push $1 \n"))
                ((equal? expr '#t)
                 (list " push $9 \n"))
                ((assoc expr env)
                 (list "# " expr "\n"
		       " mov -" (number->string (lookup expr env)) "(%rbp), %rax \n push %rax\n" ))
                (else
                 (error "parametre invalide" expr))))))

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
 ;; (trace compile-bindings)
 ;; (trace compile-let)			
 ;; (trace compile-bloc)
 ;; (trace compile-if)
 ;; (trace gen-literal)

(define (compile-program exprs)
  (list " .text\n"
        " .globl _main\n"
        " .globl main\n"
        "_main:\n"
        "main:\n"
        (map compile-bloc exprs)
        " mov $0, %rax\n"
        " ret\n"))

(define prims
  `((if       ,compile-if)
    (let      ,compile-let)
    (set!     ,compile-set!)
    ))
