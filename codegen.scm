;; fichier pour la génération du code assembleur

(include "match.scm")


(define return-count 0)
(define fs-stack '())
(define return-gensym
  (lambda ()
    (set! return-count (+ return-count 1))
    (string-append "return_" (number->string (- return-count 1)))))

(define pop-fs
  (lambda ()
    (begin
      (set! fs (car fs-stack))
      (set! fs-stack (cdr fs-stack)))))

(define push-fs
  (lambda ()
    (begin
      (set! fs-stack (cons fs fs-stack)))))

(define fs 0)

(define (compile-bloc expr)
  (if (null? expr)
      (list "")
      (let ((code
             (match (car expr)

                    ((jmp ,label)
                     (list "  jmp   " label "\n"))
                    
                    ((jmpe ,label)
                     (list "  je    " label "\n"))

                    ((lab ,label)
                     (list label ":\n"))

                    ((fs-adjust)
                     (begin
                       (set! fs (- fs 1))
                       (list "")))

                    ((check-stack-integrity ,expected-size)
                     (if (= fs expected-size)
                         (list "")
                         (let ((delta (- fs expected-size)))
                           (set! fs expected-size)
                           (debug fs expr)
                           (list "  pop   %rax        # pop result\n"
                                 "  add   $8*" delta ", %rsp  # adjust stack\n"
                                 (if (= expected-size 0)
                                     ""
                                     "  push  %rax        # push result\n")
                                 ;;"  call  print_rsp\n"))))
                                 ))))
                    
                    ((proc ,name ,nb-params)
                     (begin
		       (push-fs)
                       (set! fs (+ 1 nb-params))
                       (debug fs expr)
                       (list ".align 8\n"
                             ".quad 0\n"
                             ".quad 0\n"
                             ".byte 0\n"
                             name ":\n"
                             "  cmp   $" nb-params ", %rax\n"
                             "  jnz   nargs_error\n")))

                    ((call ,nargs)
                     (let ((retLab (return-gensym)))
                       (begin
                         (set! fs (- fs nargs))
                         (debug fs expr)
                         (list "  mov   (%rsp), %rdi\n"
                               "  lea   " retLab "(%rip), %rax\n"
                               "  push  %rax\n"
                               "  mov   $" nargs ", %rax\n"
                               "  jmp   *-1(%rdi)\n"
                               ".align 8\n"
                               ".quad 0\n"
                               ".quad 12\n"
                               ".byte 0\n"
                               retLab ":\n"))))
                    
                    ((ret ,pos)
                     (let ((old-fs fs))
                       (begin
			 (pop-fs)
			 (debug fs expr)
                         (list "  mov   8*" pos "(%rsp), %rdi\n"
			       "  mov   (%rsp), %rax\n"
			       "  add   $8*" old-fs ", %rsp\n"
			       "  push  %rax\n"
			       "  jmp   *%rdi\n"))))

                    ((push_proc ,label)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  lea   " label "(%rip), %rax\n"
                             "  push  %rax\n")))

                    ((push_lit ,val)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (cond ((number? val)
                              (list "  push  $8*" val "\n"))
                             ((null? val)
                              (list "  push  $17\n"))
                             ((char? val)
                              (list "  push  $2+8*" (char->integer val) "\n"))
                             ((boolean? val)
                              (if val
                                  (list "  push  $9\n")
                                  (list "  push  $1\n"))))))
                    
                    ((push_loc ,pos)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  push  8*" pos "(%rsp)\n")))

                    ((push_glo ,pos)
                     (begin
                       (set! fs (+ fs 1 ))
                       (debug fs expr)
                       (list "  push  glob_" pos "(%rip)\n")))
                    
                    ((pop_glo ,pos)
                     (begin
                       (set! fs (- fs 1 ))
                       (debug fs expr)
                       (list "  pop   glob_" pos "(%rip)\n")))

                    ((push_free ,pos)
                     (begin
                       (debug fs expr)
                       (list "  pop   %rdi\n"
                             "  push  8*" (+ pos 1) "-1(%rdi)\n")))

                    ((pop_free ,pos)
                     (begin
                       (set! fs (- fs 2))
                       (debug fs expr)
                       (list "  pop   %rdi\n"
                             "  pop   8*" (+ pos 1) "-1(%rdi)\n")))

                    ((close ,nfree)
                     (begin
                       (set! fs (- fs nfree))
                       (debug fs expr)
                       (list (get-fv (- nfree 1))
                             "  pop   8*2(%r10)  # ptr-code\n"
                             "  push  $0\n"
                             "  pop   8*1(%r10)  # type\n"
                             "  push  $8*" (+ nfree 1) "\n"
                             "  pop   8*0(%r10)  # longueur\n"
                             "  push  %r10\n"
                             "  add   $8*2+1, (%rsp)\n"
                             "  add   $8*" (+ nfree 3) ", %r10\n")))

                    ((push_this ,offset)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  push  8*" offset "(%rsp)\n")))

                    ((println)
                     (begin
                       (debug fs expr)
                       (list "  call  print_ln\n"
                             "  push  $10\n"
                             "  call  putchar\n"
                             "  push  $0\n")))

                    ((add)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   %rax\n"
                             "  add   %rax, (%rsp)\n")))
                    
                    ((sub)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   %rbx\n"
                             "  pop   %rax\n"
                             "  sub   %rbx, %rax\n"
                             "  push  %rax\n")))
                    ((mul)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   %rax\n"
                             "  pop   %rbx\n"
                             "  sar   $3, %rbx\n"
                             "  mul   %rbx\n"
                             "  push  %rax\n")))
                    
                    ((quotient)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   %rbx\n"
                             "  pop   %rax\n"
                             "  cqo\n"
                             "  idiv  %rbx\n"
                             "  sal   $3,%rax\n"
                             "  push  %rax\n")))
                    
                    ((remainder)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   %rbx\n"
                             "  pop   %rax\n"
                             "  cqo\n"
                             "  idiv  %rbx\n"
                             "  push  %rdx\n")))
                    
                    ((modulo)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   %rbx\n"
                             "  pop   %rax\n"
                             "  mov   %rax,%r8\n"
                             "  mov   $0,%r9\n"
                             "  cqo\n"
                             "  idiv  %rbx\n"
                             "  xor   %rbx,%r8\n"
                             "  cmovns %r9,%rbx\n"
                             "  add   %rbx,%rdx\n"
                             "  push  %rdx\n")))
                    
                    ((cmp)
                     (begin
                       (set! fs (- fs 2))
                       (debug fs expr)
                       (list "  pop   %rax\n"
                             "  pop   %rbx\n"
                             "  cmp   %rax, %rbx\n")))
                    
                    ((less?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  mov   $1, %rax\n"
                             "  mov   $9, %rbx\n"
                             "  cmovs %rbx, %rax\n"
                             "  push  %rax\n")))
                    
                    ((equal?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  mov   $1, %rax\n"
                             "  mov   $9, %rbx\n"
                             "  cmovz %rbx,%rax\n"
                             "  push  %rax\n")))

                    ((get_tag)
                     (begin
                       (debug fs expr)
                       (list "  and   $7,(%rsp)\n")))

                    ((push_tag ,tag)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  push  $" tag "\n")))

                    ((null?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  push  $17\n")))

                    ((boolean?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  mov   (%rsp), %rax\n"
                             "  and   $15, %rax\n"
                             "  push  %rax\n")))

                    ((push_heap ,size)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  mov   (%r10), %rax\n"
                             "  add   $8*" size ", %r10\n"
                             "  push  %rax\n")))
                    
                    ((cons)
                     (begin
                       (set! fs (- fs 2))
                       (list "  pop   %rbx\n"
                             "  pop   %rax\n"
                             "  mov   (%rsp), %rdi\n"
                             "  mov   %rax, (%rdi)\n"
                             "  mov   %rbx, 8(%rdi)\n")))
                    
		    ((comment ,val)
		     (list ""));;"\n# fs = " fs " (" val ")\n"))
                    
                    ((car)
                     (list "  mov   (%rsp), %rsi\n"
                           "  mov   (%rsi), %rax\n"
                           "  mov   %rax, (%rsp)\n"))

                    ((cdr)
                     (list "  mov   (%rsp), %rsi\n"
                           "  mov   8(%rsi), %rax\n"
                           "  mov   %rax, (%rsp)\n")))))
        
        (append code (compile-bloc (cdr expr))))))


(define (compile-env env)
  (if (null? env)
      '()
      (list "glob_" (number->string (cdr env)) ": .quad 0\n")))
;;(number->string (cdar env))
;;(compile-env (cdr env)))))
(define compile-args-error
  (list "\n"
        "nargs_error:\n"
	"  mov   $1, %rax\n"
	"  ret\n"))


(define (compile-program exprs lambdas)
  (begin
    (set! fs 0)
    (list ".text\n"
	  ".globl _main\n"
	  ".globl main\n"
	  "_main:\n"
	  "main:\n"
	  "push %rbp \n"
	  "mov %rsp, %rbp\n"
	  ;;"  call  print_rsp\n"
          "  push  $100*1024*1024\n"
	  "  call  mmap\n"
	  "  mov   %rax, %r10\n"  ;;registre pour les variable globales
	  (compile-bloc exprs)
	  "  mov   $0, %rax\n"
	  ;;"  call  print_rsp\n"
	  "mov %rbp, %rsp\n"
	  "pop %rbp\n"
          "  ret\n"
          "\n\n"
	  (compile-bloc lambdas)
	  compile-args-error
	  "\n\n"
          ".data\n"
          ".align 8\n"
	  (map compile-env genv))))

(define (debug fs expr)
  (display fs)
  (display "   ")
  (pp (car expr)))
  ;;#!void)

(define (get-fv i)
  (if (>= i 0)
      (string-append "  pop   8*" (number->string (+ i 3)) "(%r10)  # free-var\n"
                     (get-fv (- i 1)))
      ""))
