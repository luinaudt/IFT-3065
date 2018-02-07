;fichier pour la génération du code assembleur

(define (compile-expr expr)
  (if (and (pair? expr))
      (if (and (pair? (cadr expr)))
	  (compile-expr (cadr expr))
	  (let ((num (cadr expr)))
	    (append (list " push $" num "\n")
		    (analyse-op (car expr)))))
	  (error "unknown expression" expr))
  )
;fonction pour analyser une opération (premier élément d'une parenthèse)
(define (analyse-op expr)
  (pp expr)
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
       )
      (error "unknown operation" expr)
      )
  )
(trace compile-expr)
(trace analyse-op)
(define (compile-program exprs)
  (list " .text\n"
        " .globl _main\n"
        " .globl main\n"
        "_main:\n"
        "main:\n"
        (map compile-expr exprs)
        " mov $0, %rax\n"
        " ret\n"))
