;; fichier pour la génération du code assembleur

(include "match.scm")


(define return-count 0)
(define return-gensym
  (lambda ()
    (set! return-count (+ return-count 1))
    (string-append "return" (number->string (- return-count 1)))))



(define (compile-bloc expr fs)
  (if (null? expr)
      (list "add $8*" (number->string fs) ",%rsp \n")
      (let ((code
             (match (car expr)

                    ((jmp ,label)
                     (list "jmp " label "\n"))
                    
                    ((jmpe ,label)
                     (list "je " label "\n"))

                    ((lab ,label)
                     (list label ":\n"))

                    ((fs-adjust)
                     (begin
                       (set! fs (- fs 1))
                       (list ""))) 
                    
                    ((proc ,name ,nb-params)
                     (begin
                       (set! fs (+ fs (+ 1 nb-params)))
                       (debug fs expr)
                       (list "\n.align 8\n.quad 0\n.quad 0\n.byte 0\n"
                             name ":\n"
                             "cmp $" (number->string nb-params) ", %rax\n"
                             "jnz nargs_error\n")))

                    ((call ,nargs)
                     (let ((retLab (return-gensym)))
                       (begin
                         (set! fs (- fs nargs))
                         (debug fs expr)
                         (list "pop %rdi\n"
                               "lea " retLab "(%rip), %rax\n"
                               "push %rax\n"
                               "mov $" (number->string nargs) ", %rax\n"
                               "jmp *%rdi\n"
                               "\n.align 8\n.quad 0\n.quad 12\n.byte 0\n"
                               retLab ":\n"))))
                    
                    ((ret ,pos)
                     (let ((old-fs fs))
                       (debug fs expr)
                       (set! fs 0)
                       (list "mov 8*" (number->string pos) "(%rsp),%rdi\n"
                             "mov (%rsp), %rax\n"
                             "add $8*" (number->string old-fs) ",%rsp\n"
                             "push %rax\n"
                             "jmp *%rdi\n")))

                    ((push_proc ,label)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "lea " label "(%rip), %rax\n"
                             "push %rax\n")))

                    ((push_lit ,val)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (cond ((number? val)
                              (list "push $8*" val "\n"))
                             ((null? val)
                              (list "push $17\n"))
                             ((char? val)
                              (list "push $2+8*" (number->string (char->integer val)) "\n"))
                             ((boolean? val)
                              (if val
                                  (list "push $9\n")
                                  (list "push $1\n"))))))
                    
                    ((push_loc ,pos)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "push 8*" (number->string pos) "(%rsp)\n")))

                    ((push_glo ,pos)
                     (begin
                       (set! fs (+ fs 1 ))
                       (debug fs expr)
                       (list "push glob_" (number->string pos) "(%rip)\n")))
                    
                    ((pop_glo ,pos)
                     (begin
                       (set! fs (- fs 1 ))
                       (debug fs expr)
                       (list "pop glob_" (number->string pos) "(%rip)\n")))

                    ((push_free ,pos)
                     (begin
                       (list "pop  %rdi\n"
                             "push 8*" (number->string (+ pos 1)) "-1(%rdi)\n")))

                    ((pop_free ,pos)
                     (begin
                       (list "pop  %rdi\n"
                             "pop  8*" (number->string (+ pos 1)) "-1(%rdi)\n")))

                    ((close ,nfree)
                     (begin
                       (list "mov  $" (number->string (* (+ nfree 1) 8)) ", 8*0(%r11)\n"
                             "mov  $0, 8*1(%r11)\n"
                             (get-fv nfree)
                             "pop  $8*2($r11)\n"
                             "push %r11\n"
                             "add  $8*2+1, (%rsp)\n"
                             "add  $8*" (number->string (+ nfree 3)) ", %r11\n")))

                    ((println)
                     (begin
                       (debug fs expr)
                       (list "call print_ln \n"
                             "push $10\n"
                             "call putchar\n"
                             "push $0\n")))

                    ((add)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "pop %rax \n"
                             "add %rax, (%rsp)\n")))
                    
                    ((sub)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "pop %rbx \n"
                             "pop %rax\n"
                             "sub %rbx, %rax\n"
                             "push %rax\n")))
                    ((mul)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "pop %rax \n"
                             "pop %rbx \n"
                             "sar $3, %rbx\n"
                             "mul %rbx\n"
                             "push %rax\n")))
                    
                    ((quotient)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "pop %rbx\n"
                             "pop %rax\n"
                             "cqo \n"
                             "idiv %rbx\n"
                             "sal $3,%rax\n"
                             "push %rax\n")))
                    
                    ((remainder)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "pop %rbx\n"
                             "pop %rax\n"
                             "cqo\n"
                             "idiv %rbx\n"
                             "push %rdx\n")))
                    
                    ((modulo)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list " pop  %rbx\n"
                             " pop  %rax\n"
                             " mov  %rax,%r8\n"
                             " mov  $0,%r9\n"
                             " cqo\n"
                             " idiv %rbx\n"
                             " xor  %rbx,%r8\n"
                             " cmovns %r9,%rbx\n"
                             " add  %rbx,%rdx\n"
                             " push %rdx\n")))
                    
                    ((cmp)
                     (begin
                       (set! fs (- fs 2))
                       (debug fs expr)
                       (list "pop %rax\n"
                             "pop %rbx\n"
                             "cmp %rax, %rbx\n")))
                    
                    ((less?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "mov $1, %rax\n"
                             "mov $9, %rbx\n"
                             "cmovs %rbx, %rax\n"
                             "push %rax\n")))
                    
                    ((equal?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "mov $1, %rax\n"
                             "mov $9, %rbx\n"
                             "cmovz %rbx,%rax\n"
                             "push %rax\n")))

                    ((get_tag)
                     (begin
                       (debug fs expr)
                       (list "and $7,(%rsp)\n")))

                    ((push_tag ,tag)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "push $" tag "\n")))

                    ((null?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "push $17\n")))

                    ((boolean?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "mov (%rsp), %rax\n"
                             "and $15, %rax\n"
                             "push %rax\n")))

                    ((push_heap ,size)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "mov (%r11), %rax\n"
                             "add $8*" size ", %r11\n"
                             "push %rax\n")))
                    
                    ((cons)
                     (begin
                       (set! fs (- fs 2))
                       (list "pop %rbx\n"
                             "pop %rax\n"
                             "mov (%rsp), %rdi\n"
                             "mov %rax, (%rdi)\n"
                             "mov %rbx, 8(%rdi)\n")))

                    ((car)
                     (list "mov (%rsp), %rsi\n"
                           "mov (%rsi), %rax\n"
                           "mov %rax, (%rsp)\n"))

                    ((cdr)
                     (list "mov (%rsp), %rsi\n"
                           "mov 8(%rsi), %rax\n"
                           "mov %rax, (%rsp)\n")))))
        
        (append code (compile-bloc (cdr expr) fs)))))


(define (compile-env env)
  (if (null? env)
      '()
      (list "glob_" (number->string (cdr env)) ": .quad 0\n")))
;;(number->string (cdar env))
;;(compile-env (cdr env)))))
(define compile-args-error
  (list "\n"
        "nargs_error:\n"
	"mov $1, %rax\n"
	"ret\n"))


(define (compile-program exprs lambdas env)
  (list " .text\n"
	" .globl _main\n"
	" .globl main\n"
	"_main:\n"
	"main:\n"
	"push $100*1024*1024\n"
	"call mmap\n"
	"mov %rax, %r11\n" ;;registre pour les variable globales
	(compile-bloc exprs 0)
	" mov $0, %rax\n"
	" ret\n \n\n"
	(compile-bloc lambdas 0)
	compile-args-error
	"\n\n.data\n .align 8\n"
	(map compile-env env)))

(define (debug fs expr)
  (display fs)
  (display "   ")
  (pp (car expr)))

(define (get-fv i)
  (if (> i 0)
      (string->append "pop  8*" (number->string (+ i 3)) "(%r11)\n"
                      (get-fv (- i 1)))
      "pop  8*3(%r11)\n"))
