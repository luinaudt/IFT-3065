;; fichier pour la génération du code assembleur

(include "match.scm")

;; (define op-table
;;   '((+         (" add  %rbx, %rax\n"
;;                 " push %rax\n"))
;;     (-         (" sub  %rbx, %rax\n"
;;                 " push %rax\n"))
;;     (*         (" sar  $3, %rbx\n"
;;                 " mul  %rbx\n"
;;                 " push %rax\n"))
;;     (quotient  (" cqo\n"
;;                 " idiv %rbx\n"
;;                 " sal $3, %rax\n"
;;                 " push %rax\n"))
;;     (remainder (" cqo\n"
;;                 " idiv %rbx\n"
;;                 " push %rdx\n"))
;;     (modulo    (" cqo\n"
;;                 " mov  %rax, %r8\n"
;;                 " mov  %rbx, %r9\n"
;;                 " shr  $63, %r8\n"
;;                 " shr  $63, %r9\n"
;;                 " cmp  %r8, %r9\n"
;;                 " mov  $0, %r8\n"
;;                 " cmovne %rbx, %r8\n"
;;                 " idiv %rbx\n"
;;                 " add  %r8, %rdx\n"
;;                 " push %rdx\n"))
;;     (<         (" cmp  %rbx, %rax\n"
;;                 " mov  $1, %rax\n"
;;                 " mov  $9, %rbx\n"
;;                 " cmovs %rbx, %rax\n"
;;                 " push %rax\n"))
;;     (=         (" cmp  %rbx, %rax\n"
;;                 " mov  $1, %rax\n"
;;                 " mov  $9, %rbx\n"
;;                 " cmovz %rbx, %rax\n"
;;                 " push %rax\n"))
;;     ))


;; (define env
;;   '())

;; (define stack 0)


;; (define (lookup key dict)
;;   (let ((key-val (assoc key dict)))
;;     (if key-val
;;         (cadr key-val)
;;         #f)))

;; (define gen list)

;; ;; (define (list? lst)
;; ;;   (or (null? lst)
;; ;;       (list? (cdr lst))))

;; ;; (define (pair? p)
;; ;;   (and (not (null? p))
;; ;;        (list? p)))

;; ;; (define (number? n)
;; ;;   (and (not (null? n))
;; ;;        (not (pair? n))
;; ;;        (= (modulo n 8) 0)))

;; ;; (define (boolean? b)
;; ;;   (and (not (null? b))
;; ;;        (not (pair? b))
;; ;;        (= (modulo b 8) 1)))

;; ;; (define (char? c)
;; ;;   (and (not (null? c))
;; ;;        (not (pair? c))
;; ;;        (= (modulo c 8) 2)))


;; (define (compile-bloc exprs)
;;   (if (pair? exprs)
;;       (cons (cons " push %rbp\n mov %rsp, %rbp\n"
;; 	     (compile-expr exprs))
;;             " mov %rbp, %rsp \n pop %rbp\n")
;;       (error "unknown expression" exprs)))


;; (define (compile-expr expr)
;; ;;  (pp expr)
;;   (match expr
;; 	 (,number when (number? expr)
;; 		  (gen-literal expr))
;; 	 (($println ,expr)
;; 	  (cond ((number? expr)
;; 		 (append (gen-literal expr)
;; 			 (list " call print_word_dec\n"
;; 			       " push $10  # newline\n"
;; 			       " call putchar\n")))
;; 		((pair? expr)
;; 		 (compile-expr expr))
;; 		(else append
;; 		      (cond ((eq? expr '#f)
;; 			     (list " push '#'\n"
;; 				   " call putchar\n"
;; 				   " push 'f'\n"
;; 				   " call putchar\n"))
;; 			    ((eq? expr '#t)
;; 			     (list " push '#'\n"
;; 				   " call putchar\n"
;; 				   " push 't'\n"
;; 				   " call putchar\n"))
;; 			    ((eq? expr '())
;; 			     (list " push '('\n"
;; 				   " call putchar\n"
;; 				   " push ')'\n"
;; 				   " call putchar\n"))
;; 			    ((string? expr)
;; 			     (let ((len (string-length expr)))
;; 			       (let loop ((i 0) (code '()))
;; 				 (if (< i len)
;; 				     (loop (+ i 1)
;; 					   (append code
;; 						   (list " push $"
;; 							 (char->integer (string-ref expr i))
;; 							 "\n"
;; 							 " call putchar\n")))
;; 				     code)))))
;; 		      (list " push $10  # newline\n"
;; 			    " call putchar\n"))))

;; 	 (($+ ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list " pop  %rax\n"
;; 			" add  %rax,(%rsp)\n")))

;; 	 (($- ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list " pop  %rax\n"
;; 			" sub  %rax,(%rsp)\n")))

;; 	 (($* ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list " pop  %rax\n"
;; 			" sar  $3,$rax\n"
;; 			" imul (%rsp)\n")))

;; 	 (($quotient ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list " pop  %rbx\n"
;; 			" pop  %rax\n"
;; 			" cqo\n"
;; 			" idiv %rbx\n"
;; 			" sal  $3,%rax\n"
;; 			" push %rax\n")))

;; 	 (($remainder ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list " pop  %rbx\n"
;; 			" pop  %rax\n"
;; 			" cqo\n"
;; 			" idiv %rbx\n"
;; 			" push %rdx\n")))

;; 	 (($modulo ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list " pop  %rbx\n"
;; 			" pop  %rax\n"
;; 			" mov  %rax,%r8\n"
;; 			" mov  $0,%r9\n"
;; 			" cqo\n"
;; 			" idiv %rbx\n"
;; 			" xor  %rbx,%r8\n"
;; 			" cmovns %r9,%rbx\n"
;; 			" add  %rdx,%rbx\n"
;; 			" push %rdx\n")))

;; 	 (($< ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list (" pop  %rbx\n"
;; 			 " pop  %rax\n"
;; 			 " cmp  %rax,%rbx\n"
;; 			 " mov  $1,%rax  # false\n"
;; 			 " mov  $9,%rbx  # true\n"
;; 			 " cmovs %rbx,%rax\n" 
;; 			 " push %rax\n"))))
	 
;; 	 (($= ,expr1 ,expr2)

;; 	  (append (compile-expr expr1)
;; 		  (compile-expr expr2)
;; 		  (list (" pop  %rbx\n"
;; 			 " pop  %rax\n"
;; 			 " cmp  %rax,%rbx\n"
;; 			 " mov  $1,%rax  # false\n"
;; 			 " mov  $9,%rbx  # true\n"
;; 			 " cmovz %rbx,%rax\n"
;; 			 " push %rax\n"))))))
         
;; ;;(pp compile-expr)
;; ;; (define (compile-expr expr)
;; ;;   (cond ((or (number? expr)
;; ;; 	     (string? expr)
;; ;; 	     (equal? expr '#f)
;; ;; 	     (equal? expr '#t))
;; ;;          (gen-literal expr))
;; ;;         ((list? expr)
;; ;;          (if (null? expr)
;; ;;              (gen-list expr)
;; ;;              (let ((first (car expr)) (rest (cdr expr)))
;; ;;                (cond ((assoc first prims)
;; ;; 		      ((lookup first prims) rest))
;; ;;                      ((assoc first env)
;; ;;                       ((lookup first env) rest))
;; ;;                      ((or (assoc first op-table)
;; ;;                           (equal? first 'println))
;; ;;                       (analyse-proc expr))
;; ;;                      (else
;; ;; 		      (error "unknown expression " expr))))))
;; ;;         ((pair? expr)
;; ;;          (gen-pair expr))
;; ;;         ((assoc expr env)
;; ;;          (gen-var (lookup expr env)))
;; ;;         (else
;; ;;          (error "unknown expression" expr))))

;; (define (gen-var n)
;;   (gen " mov -" (number->string n) "(%rbp), %rax\n"
;;        " push %rax\n")
;;   )

;; (define (compile-quote expr)
;;   (if (= 1 (length expr))
;;       (gen-literal (car expr))
;;       (error ("Ill form expression quote")
;; 	     )))

;; (define (gen-literal n)
;;   (cond ((number? n)
;; 	 (gen " mov $" (number->string (* 8 n)) ", %rax\n"
;; 	      " push %rax\n"))
;; 	((equal? n '#f)
;; 	 (gen " mov $1, %rax\n"
;; 	      " push %rax\n"))
;;         ((equal? n '#t)
;;          (gen " mov $9, %rax\n"
;;               " push %rax\n"))
;;         (else
;;          (error "unsupported literal" n))))

;; ;;liste de littéraux
;; (define (gen-list lst)
;;   (pp lst)
;;   (error "gen-list not yet implemented"))

;; (define (gen-pair p)
;;   (error "gen-pair not yet implemented"))

;; ;;comilation d'un if
;; (define (compile-if exprs)
;;   (cond ((= (length exprs) 3)
;;          (let ((jmp-false (symbol->string (gensym)))
;;                (jmp-endif (symbol->string (gensym))))
;;            (gen (compile-expr (car exprs))
;; 		" pop %rax\n"
;; 		" cmp $1, %rax\n"
;;                 " je " jmp-false "\n"
;;                 (compile-expr (cadr exprs))
;;                 " jmp " jmp-endif "\n"
;;                 jmp-false ":\n"
;;                 (compile-expr (caddr exprs))
;;                 jmp-endif ":\n")))
;;         ((= (length exprs) 2)
;;          (let ((jmp-endif (symbol->string (gensym))))
;;            (gen (compile-expr (car exprs))
;;                 " cmp $9, 4(%rsp)\n"
;;                 " jne " jmp-endif "\n"
;;                 (compile-expr (cadr exprs))
;;                 jmp-endif ":\n")))
;;         (else
;;          (error "invalid construct: if"))))

;; ;;compilation pour le let
;; (define (compile-let exprs)
;;   (if (< (length exprs) 2)
;;       (error "invalid construct: let")
;;       (let ((old-env env) (old-stack stack) (ret '()))
;;         ;; ajoute à l'environnement les nouveaux bindings
;;         ;;        (compile-bindings (car exprs) '())
	
;;         ;; compile le corps du let
;; 	;; on sauvegarde l'environnement en mettant le rsp et en gardant rbp
;; 	;; TODO: ajouter la sauvegarde de l'environnement avec des registres ?
;; 	(begin (set! ret (cons (cons (if (= 0 old-stack)
;; 					 " push %rbp \n mov %rsp, %rbp\n"
;; 					 "")
;; 				     (compile-bindings (car exprs) '()))
;; 			       (cons (map compile-expr (cdr exprs))
;; 				     (if (= 0 old-stack)
;; 					 " mov %rbp, %rsp \n pop %rbp\n push %rax\n"
;; 					 (list " mov %rax, -" (+ old-stack 8) "(%rbp)\n") ))))
;; 	       ;; retourne l'environnement à son état original
;; 	       (set! env old-env)
;; 	       (set! stack old-stack)
;; 	       ret )
;; 	)))



;; ;; Pour le let on doit avoir une stack au demarrage.
;; ;; cela implique un partage de l'environnement. Doit on faire une structure env pour une meilleur evolution

;; ;; traitement des bindings du let 
;; (define (compile-bindings bindings let-env)
;;   (if (not (null? bindings))
;;       (let ((first (car bindings)) (rest (cdr bindings)))
;;         (cond ((assoc (car first) let-env)
;;                (error "duplicate variable in let bindings"))
;;               ((or (pair? first) (= (length first) 2))
;;                (let ((ret '()) (new-bind '()))
;; 		 (begin (set! ret (compile-expr (cadr first)))
;; 			(set! new-bind (list (car first)
;; 					     (+ 8 stack)))
;; 			(set! stack (+ 8 stack))

;; 			(set! env (if (null? env) (cons new-bind '()) (cons new-bind env)))
;; 			(set! ret (cons ret (compile-bindings rest
;;                                                               (cons new-bind let-env))))
;; 			ret)

;;                  ;; on modifie l'environnement à la fin car un binding
;;                  ;; ne doit pas influencer les autres bindings
;; 		 ;;(set! env (cons new-bind env))))
;; 		 ))
;;               (else
;;                (error "invalid binding construct: let"))))
;;       '() ))

;; ;;assignation des variables
;; (define (compile-set! exprs)
;;   (error "set! not supported yet"))

;; ;; analyse procedure
;; (define (analyse-proc expr)
;;   (if (= (length expr) 2)
;;       (cons (analyse-operand (cadr expr))
;; 	    (analyse-op (car expr)))
;;       (if (= (length expr) 3)
;; 	  (cons  (cons (analyse-operand (cadr expr))
;;                        (analyse-operand (caddr expr)))
;; 		 (analyse-op (car expr)))
;; 	  (error "expression non valide" expr))))

;; ;; fonction analyser les opérande d'une fonction
;; (define (analyse-operand expr)
;;   (if (null? expr)
;;       '()
;;       (if (pair? expr)
;;           (compile-expr expr)
;;           (cond ((number? expr)
;;                  (list " mov $" expr ",%rax\n"
;;                        " sal $3, %rax\n"
;;                        " push %rax \n"))
;;                 ((equal? expr '#f)
;;                  (list " push $1 \n"))
;;                 ((equal? expr '#t)
;;                  (list " push $9 \n"))
;;                 ((assoc expr env)
;;                  (list "# " expr "\n"
;; 		       " mov -" (number->string (lookup expr env)) "(%rbp), %rax \n push %rax\n" ))
;;                 (else
;;                  (error "parametre invalide" expr))))))

;; ;; fonction pour analyser une opération (premier élément d'une parenthèse)
;; (define (analyse-op expr)
;;   (cond ((equal?  expr 'println)
;;          (list  " call print_ln\n"
;;                 " push $10\n"
;;                 " call putchar\n"))
;;         ((lookup expr op-table)
;;          (list " pop %rbx\n"
;;                " pop %rax\n"
;;                (lookup expr op-table)))
;;         (else
;;          (error "unknown operation" expr))))

 ;; (trace compile-expr)
 ;; (trace analyse-op)
 ;; (trace analyse-operand)
 ;; (trace analyse-proc)
 ;; (trace compile-bindings)
 ;; (trace compile-let)			
 ;; (trace compile-bloc)
 ;; (trace compile-if)
 ;; (trace gen-literal)
(define (compile-bloc expr)
  (match expr
         ((proc ,name ,nbparams)
	  (list ;;".align 3\n .quad 0 \n .quad 0 \n .byte 0\n"
		(symbol->string name) ":\n"
		"cmp $" (number->string nbparams) ", %rax\n"
		"jnz nargs_error\n"))
	 
	 ((ret ,pos)
	  (list "mov 8*" (number->string pos) "(%rsp),%rdi \n"
		"mov (%rsp), %rax \n"
		"#add $8*0"  ",%rsp\n"
		"push %rax \n"
		"jmp *%rdi\n"))
	 
	 ((pop_glo ,pos)
	  (list "pop %rax\n"
		"mov %rax," (number->string (* 8 pos)) "(%r11)\n"))

         ((push_glo ,pos)
	  (list "mov " (number->string (* 8 pos)) "(%r11), %rax\n"
		"push %rax\n"))

         ((push_lit ,val)
	  (list "push $8*" val "\n"))

	 ((push_proc ,lab)
	  (list "lea " (symbol->string lab) "(%rip), %rax\n"
		"push %rax\n"))
	 
         ((push_loc ,pos)
	  (list "#todo\n"))

         ((add)
	  (list "pop %rax \n"
		"add %rax, (%rsp)\n"))
	 ((mul)
	  (list "pop %rax \n"
		"pop %rbx \n"
		"sar $3, %rbx\n"
		"mul %rbx\n"
		"push %rax\n"))
	 ((quotient)
	  (list "pop %rax\n"
		"pop %rbx\n"
		"cqo \n"
		"idiv %rbx\n"
		"sal $3,%rax\n"
		"push %rax\n"))
	 ((remainder)
	  (list "pop %rax\n"
		"pop %rbx\n"
		"cqo\n"
		"idiv %rbx\n"
		"push %rdx\n"))
	 ((modulo)
	  (list "pop %rax\n"
		"pop %rbx\n"
		"cqo\n"
		"mov %rax, %r8\n"
		"mov %rbx, %r9\n"
		"shr $63,%r8\n"
		"shr $63,%r9\n"
		"cmp %r8, %r9\n"
		"cmovne %rbx, %r8\n"
		"idiv %rbx\n"
		"add %r8, %rdx\n"
		"push %rdx\n"))
	 ((less?)
	  (list "cmovs %rbx, %rax\n"
		"push %rax\n"))
	 ((cmp)
	  (list "pop %rax\n"
		"pop %rbx\n"
		"cmp %rax, %rbx\n"
		"mov $1, %rax\n"
		"mov $9, %rbx\n"))
	 ((equal?)
	  (list "cmovz %rbx,%rax\n"
		"push %rax"))
         ((sub)
          (list "pop %rax \n"
                "sub %rax, (%rsp)\n"))

         ((println)
	  (list "mov (%rsp),%rax\n"
		"sar $3, %rax\n"
		"mov %rax, (%rsp)\n"
		"call print_word_dec \n"
		"push $10\n"
		"call putchar\n"))))

(define (compile-env env)
  (if (null? env)
      '()
      (list "glob_" (number->string (cdr env)) ": .quad 0\n")))
;;(number->string (cdar env))
;;(compile-env (cdr env)))))
(define compile-args-error
  (list "nargs_error:\n"
	"mov $1, %rax\n"
	"ret \n"
	""))


(define (compile-program exprs lambdas env)
  (list " .text\n"
        " .globl _main\n"
        " .globl main\n"
        "_main:\n"
        "main:\n"
	;;"push $100*1024*1024\n" ;;registre pour le tas
	;;"call mmap\n"
	;;"mov %rax, %r10\n"
	"push $100*1024*1024\n"
	"call mmap\n"
	"mov %rax, %r11\n" ;;registre pour les variable globales
        (map compile-bloc exprs)
        " mov $0, %rax\n"
        " ret\n"
	(map compile-bloc lambdas)
	compile-args-error
	".data\n"
	(map compile-env env)))
(trace compile-bloc)
;; (define prims
;;   `((if       ,compile-if)
;;     (let      ,compile-let)
;;     (set!     ,compile-set!)
;;     (quote    ,compile-quote)
;;     ))
