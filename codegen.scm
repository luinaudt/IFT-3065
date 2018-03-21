;; fichier pour la génération du code assembleur

(include "match.scm")


(define return-count 0)
(define return-gensym
  (lambda ()
    (set! return-count (+ return-count 1))
    (string-append "return" (number->string return-count))))

 

(define (compile-bloc expr fs)
  (if (null? expr)
      (list "add $8*" (number->string fs) ",%rsp \n")
      (let ((code
	     (match (car expr)
		    ((proc ,name ,nbparams)
		     (begin
		       (set! fs (+ fs (+ 1 nbparams)))
		       (list ".align 8\n .quad 0 \n .quad 0 \n .byte 0\n"
			     name ":\n"
			     "cmp $" (number->string nbparams) ", %rax\n"
			     "jnz nargs_error\n")))

		    ((call ,nargs)
		     (let ((retLab (return-gensym)))
		       (begin
			 (set! fs (- fs nargs))
			 (list "pop %rdi\n"
			       "lea " retLab "(%rip), %rax\n"
			       "push %rax\n"
			       "mov $" (number->string nargs) ", %rax \n"
			       "jmp *%rdi\n"
			       ".align 8\n .quad 0\n .quad 12 \n .byte 0\n"
			       retLab ":\n"))))
		    
		    ((ret ,pos)
		     (let ((old-fs fs))
		       (begin
			 (set! fs 0)
			 (list "mov 8*" (number->string pos) "(%rsp),%rdi \n"
			       "mov (%rsp), %rax \n"
			       "add $8*" (number->string old-fs)  ",%rsp\n"
			       "push %rax \n"
			       "jmp *%rdi\n"))))

		    ((push_proc ,lab)
		     (begin
		       (set! fs (+ fs 1 ))
		       (list "lea " lab "(%rip), %rax\n"
			     "push %rax\n")))
		    
		    ((pop_glo ,pos)
		     (begin
		       (set! fs (- fs 1 ))
		       (list "pop glob_" (number->string pos) "(%rip)\n")))

		    ((push_glo ,pos)
		     (begin
		       (set! fs (+ fs 1 ))
		       (list "push glob_" (number->string pos) "(%rip)\n")))

		    ((push_lit ,val)
		     (begin
		       (set! fs (+ fs 1 ))
		       (cond ((number? val)
			      (list "push $8*" val "\n"))
			     ((char? val)
			      (list "push $2+8*" (number->string (char->integer val)) "\n"))
			     ((boolean? val)
			      (if val
				  (list "push $9\n")
				  (list "push $1\n")
			     )))))
		       
		    ((push_loc ,pos)
		     (begin
		       (set! fs (+ fs 1 ))
		       (list "push 8*" (number->string pos) "(%rsp)\n")))

		    ((add)
		     (begin
		       (set! fs (- fs 1))
		       (list "pop %rax \n"
			     "add %rax, (%rsp)\n")))
		    ((mul)
		     (begin
		       (set! fs (- fs 1 ))
		       (list "pop %rax \n"
			     "pop %rbx \n"
			     "sar $3, %rbx\n"
			     "mul %rbx\n"
			     "push %rax\n")))
		    ((quotient)
		     (begin
		       (set! fs (- fs 1 ))
		       (list "pop %rax\n"
			     "pop %rbx\n"
			     "cqo \n"
			     "idiv %rbx\n"
			     "sal $3,%rax\n"
			     "push %rax\n")))
		    ((remainder)
		     (begin
		       (set! fs (- fs 1 ))
		       (list "pop %rax\n"
			     "pop %rbx\n"
			     "cqo\n"
			     "idiv %rbx\n"
			     "push %rdx\n")))
		    ((modulo)
		     (begin
		       (set! fs (- fs 1 ))
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
			     "push %rdx\n")))
		    ((less?)
		     (begin
		       (set! fs (+ fs 1 ))
		       (list "cmovs %rbx, %rax\n"
			     "push %rax\n")))
		    ((cmp)
		     (begin
		       (set! fs (- fs 2 ))
		       (list "pop %rax\n"
			     "pop %rbx\n"
			     "cmp %rax, %rbx\n"
			     "mov $1, %rax\n"
			     "mov $9, %rbx\n")))
		    ((equal?)
		     (begin
		       (set! fs (+ fs 1 ))
		       (list "cmovz %rbx,%rax\n"
			     "push %rax\n")))
		    ((sub)
		     (begin
		       (set! fs (- fs 1))
		       (list "pop %rbx \n"
			     "pop %rax\n"
			     "sub %rbx, %rax\n"
			     "push %rax\n")))

		    ((println)
		     (begin
		       ;;(set! fs (+ fs 1))
		       (list "call print_ln \n"
			     "push $10\n"
			     "call putchar\n"
			     "push $0\n"))))))
	(append code (compile-bloc (cdr expr) fs)))))


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


;;(trace compile-bloc)
;;(trace compile-expr)
;; (define prims
;;   `((if       ,compile-if)
;;     (let      ,compile-let)
;;     (set!     ,compile-set!)
;;     (quote    ,compile-quote)
;;     ))
