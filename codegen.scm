;; fichier pour la génération du code assembleur

(include "match.scm")


(define return-count -1)
(define spec-lab -1)
(define fs-stack '())

(define spec-lab-gensym
  (lambda ()
    (set! spec-lab (+ spec-lab 1))
    (string-append "spec_" (number->string spec-lab))))
(define return-gensym
  (lambda ()
    (set! return-count (+ return-count 1))
    (string-append "return_" (number->string return-count))))

;;fonction pour l'ajout de string
(define push-char
  (lambda (str)
    (letrec ((loop (lambda (str pos)
		     (append (list "push  $2+8*" (char->integer (string-ref str pos)) "\n"
				   "pop (%r10)\n"
				   "add $8, %r10\n")
			     (if (= (string-length str)
				    (+ pos 1))
				 '()
				 (loop str (+ pos 1)))))))
      (loop str 0))))

(define push-string
  (lambda (str)
    (let ((len (string-length str)))
      (append (list "push $8*" (number->string len) "\n"
		    "pop (%r10)\n"
		    "add $8, %r10\n")
	      (if (= 0 len)
		  '()
		  (push-char str))))))


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
                     (let ((delta (- fs expected-size)))
                       (begin
                         (set! fs expected-size)
                         (debug fs expr)
                         (cond ((= delta 0)
                                (list ""))
                               ((= expected-size 0)
                                (list "  add   $8*" delta ", %rsp  # adjust to " fs "\n"))
                               (else
                                (list "  pop   %rax        # pop result\n"
                                      "  add   $8*" delta ", %rsp  # adjust to " fs "\n"
                                      "  push  %rax        # push result\n"))))))
                    
                    ((proc ,name ,nb-params)
                     (begin
                       ;;(set! fs (- fs nb-params))
                       (push-fs)
                       (set! fs (+ 1 nb-params))
                       (debug fs expr)
                       (list "\n"
                             ".align 8\n"
                             ".quad 0\n"
                             ".quad 0\n"
                             ".byte 0\n"
                             name ":\n"
                             "  cmp   $" nb-params ", %rax\n"
                             "  jnz   nargs_error\n")))
		    ((proc_reste ,name ,nb-params)
		     (begin
		       (push-fs)
                       (set! fs (+ 1 nb-params))
                       (debug fs expr)
		       (let ((lab (spec-lab-gensym))
			     (labf (spec-lab-gensym)))
			 (list "\n"
			       ".align 8\n"
			       ".quad 0\n"
			       ".quad 0\n"
			       ".byte 0\n"
			       name ":\n"
			       "  cmp   $" (- nb-params 1) ", %rax\n"
			       "  jl   nargs_error\n" ;; traiter cas vide
			       "  mov %rsp, %rbx \n"
			       "  add $8*2,%rbx\n" ;premier argument
			       "  mov %rax, %rcx\n"
			       "  sub $" (- nb-params 1) ", %rcx\n"
			       "  sal $3, %rcx\n"
			       "  add %rbx, %rcx\n" ;;calcul fin
			       "  push $17 \n"
			       "  cmp   $" (- nb-params 1) ", %rax\n"
			       "  je " labf "\n"
			       lab ":\n" ;;tant que rbx<>rcx faire liste
			       "  push  (%rbx)\n"
			       "  pop   (%r10)\n"
			       "  pop   8(%r10)\n"
			       "  lea   6(%r10), %rdx\n"
			       "  push  %rdx\n"
			       "  add   $16, %r10   # update heap-ptr\n"
			       "  add $8, %rbx\n"
			       "  cmp %rbx, %rcx\n"
			       "  jne " lab "\n"
			       labf ":\n"
			    ;;fin   
			       "  pop %rdx\n"
			       "  pop %rcx\n"
			       "  pop %rbx\n"
			       "  sub $" (- nb-params 1) ", %rax\n"
			       "  sal $3, %rax\n"
			       "  add %rax, %rsp\n"
			       "  mov (%rsp), %rax\n"
			       "  push  %rdx\n";; 2*8(%rsp)\n"
			       "  push %rbx\n"
			       "  push %rcx\n"
			       
			       ))))
		    
                    ((call ,nargs)
                     (let ((retLab (return-gensym)))
                       (begin
                         (set! fs (- fs (- nargs 1)))
                         (debug fs expr)
                         (list "  mov   (%rsp), %rdi\n"
                               "  lea   " retLab "(%rip), %rax\n"
                               "  push  %rax\n"
                               "  mov   $" nargs ", %rax\n"
                               "  jmp   *-7(%rdi)\n"
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
                       (cond ((equal? val #!void)
			      (list "  push $25\n"))
			     ((symbol? val)
			      (append (list "push %r10\n"
                                            "add $4, (%rsp)\n"
					    "# symbole  " (symbol->string val) "\n")
                                      (list (push-string (symbol->string val)))))
			     ((number? val)
                              (list "  push  $8*" val "\n"))
                             ((null? val)
                              (list "  push  $17\n"))
                             ((char? val)
                              (list "  push  $2+8*" (char->integer val) "\n"))
			     ((string? val)
                              (append (list "push %r10\n"
                                            "add $3, (%rsp)\n")
				      ;;"# string  " val "\n")
                                      (list (push-string val))))
                             ((boolean? val)
                              (if val
                                  (list "  push  $9\n")
                                  (list "  push  $1\n"))))))
                    
                    ((push_loc ,pos)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  push  8*" pos "(%rsp)\n")))
                    
                    ((pop_loc ,pos)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   8*" pos "(%rsp)\n")))

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
                             "  push  8*" (+ pos 1) "-7(%rdi)\n")))

                    ((pop_free ,pos)
                     (begin
                       (set! fs (- fs 2))
                       (debug fs expr)
                       (list "  pop   %rdi\n"
                             "  pop   8*" (+ pos 1) "-7(%rdi)\n")))

                    ((close ,nfree)
                     (begin
                       (set! fs (- fs nfree))
                       (debug fs expr)
                       (list (get-fv (- nfree 1))
                             "  pop   8*2(%r10)  # code-ptr\n"
                             "  push  $0\n"
                             "  pop   8*1(%r10)  # type\n"
                             "  push  $8*" (+ nfree 1) "\n"
                             "  pop   8*0(%r10)  # longueur\n"
                             "  push  %r10\n"
                             "  add   $8*2+7, (%rsp)\n"
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
                    ((assign_vec)
		     (begin
		       (set! fs (- fs 1))
		       (let ((lab (spec-lab-gensym))
			     (labf (spec-lab-gensym)))
			 (list "  mov 8(%rsp), %rax\n #vecteur creation \n" ;;vector
			       "  mov (%rsp), %rbx\n" ;; val
			       "  add $-5, %rax\n"
			       "  mov (%rax), %rcx\n" ;; length
			       "  cmp $0, %rcx\n"    ;; 0 pas d'assignation
			       "  je " labf "\n"
			       "  add %rax, %rcx\n" ;;addr fin
			       lab ":\n"
			       "  mov %rbx, (%rcx)\n"
			       "  sub $8, %rcx\n"
			       "  cmp %rcx, %rax\n"
			       "  jne " lab "\n"
			       labf ":\n"
			       "  add $8, %rsp\n"))))
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
                    ((getchar)
		     (let ((lab (spec-lab-gensym)))
		       (begin (set! fs (+ fs 1))
			      (list "  call getchar\n"
				    "  mov $0, %rbx\n"
				    "  cmp $-1, %rax\n"
				    "  cmovz %rbx, %rax\n"
				    "  je  " lab "\n"
				    "  sal $3, %rax\n"
				    "  add $2,%rax\n"
				    lab ":\n"
				    "  push %rax\n"))))
		    ((putchar)
		     (begin (set! fs (- fs 1))
			    (list "  pop %rax\n"
				  "  sar $3, %rax\n"
				  "  push %rax\n"
				  "  call putchar\n")))
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
                       (list "  pop   %rax\n"
                             "  and   $7, %rax\n"
                             "  push  %rax\n")))

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
                    
		    ((save-cont ,new-fs)
		     (begin
		       (set! fs new-fs)
		       (list "push %rbp\n mov %rsp, %rbp\n")))

		    ((rest-cont ,new-fs)
		     (begin
		       (set! fs new-fs)
		       (list "pop %rax \n mov %rbp, %rsp\n pop %rbp\n push %rax\n")))
                    
                    ((boolean?)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  mov   (%rsp), %rax\n"
                             "  and   $15, %rax\n"
                             "  push  %rax\n")))
		    ((set_heap)
		     (begin
		       (set! fs (- fs 1))
		       (list "pop %r10\n")))
		    
		    ((push_mem)
		     (list "pop %rax\n"
			   "push (%rax)\n"))

		    ((pop_mem)
		     (begin
		       (set! fs (- fs 2))
		       (list "pop %rax \n"
			     "mov (%rax), %rbx\n"
			     "pop (%rax)\n")))
		    
                    ((get_heap)
                     (begin
                       (set! fs (+ fs 1))
                       (debug fs expr)
                       (list "  push   %r10\n")))
                    
                    ((cons)
                     (begin
                       (set! fs (- fs 1))
                       (debug fs expr)
                       (list "  pop   8(%r10)\n"
                             "  pop   (%r10)\n"
                             "  lea   6(%r10), %rax\n"
                             "  add   $16, %r10   # update heap-ptr\n"
                             "  push  %rax\n")))

                    ((car)
                     (begin
                       (debug fs expr)
                       (list "  pop   %rax\n"
                             "  push  -6(%rax)\n")))

                    ((cdr)
                     (begin
                       (debug fs expr)
                       (list "  pop   %rax\n"
                             "  push  2(%rax)\n")))

                    ((set-car!)
                     (begin
                       (set! fs (- fs 2))
                       (debug fs expr)
                       (list "  pop   %rdi\n"
                             "  pop   -6(%rdi)\n")))

                    ((set-cdr!)
                     (begin
                       (set! fs (- fs 2))
                       (debug fs expr)
                       (list "  pop   %rdi\n"
                             "  pop   2(%rdi)\n")))
                    
		    ((comment ,val)
		     (list "")
                     ;;(list "\n# fs = " fs " (" val ")\n")
		     ))))

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
	"  mov %rbp, %rsp\n"
	"  pop %rbp\n"
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
	  ;; "  call  print_rsp\n"
          "  push  $20*1024*1024\n"
	  "  call  mmap\n"
	  "  mov   %rax, %r10\n"  ;;registre pour les variable globales
	  (compile-bloc exprs)
	  "  mov   $0, %rax\n"
	  ;; "  call  print_rsp\n"
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
  ;; (display fs)
  ;; (display "   ")
  ;; (pp (car expr)))
  #!void)

(define (get-fv i)
  (if (>= i 0)
      (string-append "  pop   8*" (number->string (+ i 3)) "(%r10)  # free-var\n"
                     (get-fv (- i 1)))
      ""))
