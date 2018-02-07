;fichier pour la génération du code assembleur

;on compile un bloc complet
(define (compile-expr expr)
  (if (pair? expr)
      (append (compile-fonc expr)
	      "pop %rax\n")
      (error "unknown expression" expr))
  )

;on compile une fonction unique
(define (compile-fonc expr)
  (append (analyse-param (cdr expr))
	  (analyse-op (car expr))
	  )
  )


;fonction mettre les paramètres de fonction
(define (analyse-param expr)
  (if (pair? expr)
      (if (pair? (car expr))
	  (append (compile-fonc (car expr))
		  (analyse-param (cdr expr)))
	  (if (number? (car expr))
	      (append (list " push $" (car expr) "\n")
		      (analyse-param (cdr expr)))		  
	  (error "parametre invalide" expr)
	  ))
      (if (not (null? expr))
	  (list "push $" expr "\n")
	  '()
	  )
      )
  )


(define op-table '((+         (" add  %rbx, %rax\n"
                               " push %rax\n"))
                   (-         (" sub  %rbx, %rax\n"
                               " push %rax\n"))
                   (*         (" mul  %rbx\n"
                               " push %rax\n"))
                   (quotient  (" cqo\n"
                               " idiv %rbx\n"
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


;fonction pour analyser une opération (premier élément d'une parenthèse)
(define (analyse-op expr)
  (cond ((equal?  expr 'println)
         (list  " call print_word_dec\n"
                " push $10\n"
                " call putchar\n"
		" push $0\n"))
        ((assoc expr op-table)
         (list " pop %rbx\n"
               " pop %rax\n"
               (cadr (assoc expr op-table))))
        (else
         (error "unknown operation" expr))))

;(trace compile-expr)
;(trace analyse-op)
;(trace analyse-param)
(define (compile-program exprs)
  (list " .text\n"
        " .globl _main\n"
        " .globl main\n"
        "_main:\n"
        "main:\n"
        (map compile-expr exprs)
        " mov $0, %rax\n"
        " ret\n"))
