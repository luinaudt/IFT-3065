;fichier pour la génération du code assembleur

(define (compile-expr expr)
  (if (pair? expr)
      (append (analyse-param (cdr expr))
	      (analyse-op (car expr)))
      (error "unknown expression" expr))
  )


;fonction mettre les paramètres de fonction
(define (analyse-param expr)
  (if (pair? expr)
      (if (pair? (car expr))
	  (append (compile-expr (car expr))
		  (analyse-param (cdr expr)))
	  (if (number? (car expr))
	      (append (list " push $" (car expr) "\n")
		      (analyse-param (cdr expr)))		  
	  (error "unvalid param" expr)
	  ))
      (if (not (null? expr))
	  (list "push $" expr "\n")
	  '()
	  )
      )
  )


(define op-table `((+        ,(string-append "    add  %rax, %rbx\n"
                                             "    push %rbx\n"))
                   (-        ,(string-append "    sub  %rax, %rbx\n"
                                             "    push %rbx\n"))
                   (*        ,(string-append "    mul  %rbx\n"
                                             "    push %rax\n"))
                   (quotient ,(string-append "    idiv %rbx\n"
                                             "    push %rax\n"))
                   (modulo   ,(string-append "    idiv %rbx\n"
                                             "    push %rdx\n"))))


;fonction pour analyser une opération (premier élément d'une parenthèse)
(define (analyse-op expr)
  (cond ((equal?  expr 'println)
         (list  " call print_word_dec\n"
                " push $10\n"
                " call putchar\n"))
        ((assoc expr op-table)
         (list " pop %rax\n"
               " pop %rbx\n"
               (cadr (assoc expr op-table))))
        (else
         (error "unknown operation" expr))))
      ;; (if (equal? expr '+)
      ;;     (list " pop %rax\n"
      ;;      	   " pop %rbx \n "
      ;;           " add %rax, %rbx \n"
      ;;   	   " push %rbx \n"
      ;;   	)
      ;;     (error "unknown operation" expr)
      ;;     )
      ;; )
;;)


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
