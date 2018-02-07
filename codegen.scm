;fichier pour la génération du code assembleur

(define (compile-expr expr)
  (if (and (pair? expr)
           (equal? (car expr) 'println)
           (pair? (cdr expr))
           (number? (cadr expr))
           (null? (cddr expr)))

      (let ((num (cadr expr)))
        (list " push $" num "\n"
              " call print_word_dec\n"
              " push $10\n"
              " call putchar\n"))
      (error "unknown expression" expr))
)

(define (compile-program exprs)
  (list " .text\n"
        " .globl _main\n"
        " .globl main\n"
        "_main:\n"
        "main:\n"
        (map compile-expr exprs)
        " mov $0, %rax\n"
        " ret\n"))
