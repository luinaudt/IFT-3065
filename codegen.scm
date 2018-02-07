;fichier pour la génération du code assembleur

(define (compile-expr expr)
  (if (and (pair? expr))
     ; (if (pair? (cadr expr))
;	  (compile-expr (cadr expr))
	  (append (analyse-param (cdr expr))
		  (analyse-op (car expr)))
	  (error "unknown expression" expr))
  )


;fonction mettre les paramètres de fonction
(define (analyse-param expr)
  (if (pair? expr)
      (if (pair? (car expr))
	  (compile-expr (car expr))
	  (if (number? (car expr))
	      (append (list " push $" (car expr) "\n")
		      (analyse-param (cdr expr)))		  
	  (error "unvalide param" expr)
	  ))
      (if (not (null? expr))
	  (list "push $" expr "\n")
	  '()
	  )
      )
 )

;fonction pour analyser une opération (premier élément d'une parenthèse)
(define (analyse-op expr)
  (if (equal?  expr 'println)
      (list  " call print_word_dec\n"
             " push $10\n"
             " call putchar\n")
      (if (equal? expr '+)
	  (list " pop %rax\n"
		" pop %rbx \n "
	        " add %rax, %rbx \n"
		" push %rbx \n"
		)
	  (error "unknown operation" expr)
	  )
      )
  )
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
