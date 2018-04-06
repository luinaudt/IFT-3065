;;The intermediate code generation part
;;This part takes as input an AST
;;generate a standard IR

;;the ast is a scheme list
;;it should contain unique symbols.
(include "match.scm")

;; Intermediate code
;; Fonction pour génération de la représentation intermdiaire.
;; retourne une liste AST avec le langage intermédiaire
(include "environnement.scm")
(include "match.scm")


(define lambda-env '())
(define env-ir '())
(define taille-glob 0)
(define fs 0)

(define (compile-ir-bloc expr env)
  (if (null? expr)
      '()
      (append (compile-ir (car expr) env)
              (compile-ir-bloc (cdr expr) env))))

(define (compile-ir expr env)
  ;;(pp expr)
  (if (null? expr)
      '()
      (match expr
             
	     ((define ,var-name ,e1)
	      (let* ((var-val (assoc var-name env-ir))
                     (ir-code
                      (if var-val
                          (begin
                            (append (compile-ir e1 env)
                                    (list `(pop_glo ,(cdr var-val)))))
                          (begin
                            (set! env-ir (env-extend env-ir `(,var-name) `(,taille-glob)))
                            (set! taille-glob (+ taille-glob 1))
                            (append (compile-ir e1 env)
                                    (list `(comment ,(symbol->string var-name))
					  `(pop_glo ,(- taille-glob 1))))))))
                (set! fs (- fs 1))
                ir-code))
	     
             ((lambda ,params . ,body)
              (let* ((proc-name (lambda-gensym))
                     (nb-params (length params))
                     (range (let loop ((x 0))
                              (if (< x nb-params) 
                                  (cons x (loop (+ x 1)))
                                  '())))
                     (loc-env (map cons params (reverse range)))
                     (new-env (append loc-env env))
                     (old-fs fs))
                (set! fs (+ nb-params 1))  ;; params + return address
                (set! lambda-env (append (list `(proc ,proc-name ,nb-params))
                                         (compile-ir-bloc body new-env)
                                         (list `(ret ,(- fs (+ nb-params 1))))
                                         lambda-env))
                (set! fs (+ old-fs 1))  ;; add 1 for lambda-expression address
                (list `(comment ("proc" ,proc-name)) `(push_proc ,proc-name))))

             ((make-closure ,code . ,fv)
              (begin
                (set! fs (+ fs 2))
		;;(set! lambda-env (append (list `(proc ,(lambda-gensym) 
		;;		  (compile-ir code env)))
                (append (compile-ir code env)
			(list '(comment "make closure"))
                        (list `(close ,(length fv))))))

             ((closure-code $clo)
              (begin
                (set! fs (+ fs 1))
                (list '(comment "closure-code")
		       `(push_this ,(- fs 1)))))

             ((closure-ref $this ,pos)
              (begin
                (set! fs (+ fs 2))
                (list `(push_this ,fs)
                      `(push_free ,pos))))

             (($cons ,e1 ,e2)
              (begin
                (set! fs (+ fs 1))
                (append (list '(push_heap 2))
                        (compile-ir e1 env)
                        (compile-ir e2 env)
                        (list '(cons)))))

             (($car ,p)
              (begin
                (set! fs (+ fs 1))
                (append (compile-ir p env)
                        (list '(car)))))

             (($cdr ,p)
              (begin
                (set! fs (+ fs 1))
                (append (compile-ir p env)
                        (list '(cdr)))))
             
	     ((if ,cond ,E0)
              (compile-ir `(if ,cond ,E0 #!void)))
             ;;(let ((labend (label-gensym)))
             ;;  (append (compile-ir cond env)
             ;;          (compile-ir '#f '())
             ;;          (list '(cmp))
             ;;  	 (list `(jmpe ,labend))
             ;;          (compile-ir E0 env)
             ;;          (list `(lab ,labend)))))
             
	     ((if ,cond ,E0 ,E1)
	      (let ((labfalse (label-gensym)) (labend (label-gensym)))
		(append (compile-ir cond env)
			(compile-ir '#f '())
			(list '(cmp))
			(list `(jmpe ,labfalse))
			(compile-ir E0 env)
			(list `(jmp ,labend))
			(list `(lab ,labfalse)) ;;faux
			(compile-ir E1 env)
			(list `(lab ,labend))   ;;fin
                        (list '(fs-adjust)))))
             
	     (($println ,ex)
              (begin
                (append (compile-ir ex env)
                        (list '(println)))))
             
             (($+ ,p1 ,p2)
              (begin
                (set! fs (- fs 1))
                (append (compile-ir p1 env)
                        (compile-ir p2 env)
                        (list '(add)))))
             
             (($- ,p1 ,p2)
              (begin
                (set! fs (- fs 1))
                (append (compile-ir p1 env)
                        (compile-ir p2 env)
                        (list '(sub)))))
             
             (($* ,p1 ,p2)
              (begin
                (set! fs (- fs 1))
                (append (compile-ir p1 env)
                        (compile-ir p2 env)
                        (list '(mul)))))
             
             (($quotient ,p1 ,p2)
              (begin
                (set! fs (- fs 1))
                (append (compile-ir p1 env)
                        (compile-ir p2 env)
                        (list '(quotient)))))
             
             (($modulo ,p1 ,p2)
              (begin
                (set! fs (- fs 1))
                (append (compile-ir p1 env)
                        (compile-ir p2 env)
                        (list '(modulo)))))
             
             (($= ,p1 ,p2)
              (begin
                (set! fs (- fs 1))
                (append (compile-ir p1 env)
                        (compile-ir p2 env)
                        (list '(cmp))
                        (list '(equal?)))))
             
             (($< ,p1 ,p2)
              (begin
                (set! fs (- fs 1))
                (append (compile-ir p1 env)
                        (compile-ir p2 env)
                        (list '(cmp))
                        (list '(less?)))))
             
             (($number? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 0))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($boolean? ,e1)
              (append (compile-ir e1 env)
                      (list '(boolean?)
                            '(cmp)
                            '(equal?))))
             
             (($char? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 2))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($string? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 3))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($pair? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 6))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($procedure? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 7))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($null? ,e1)
              (append (compile-ir e1 env)
                      (list '(null?)
                            '(cmp)
                            '(equal?))))

             (($eq? ,e1 ,e2)
              (append (compile-ir e1 env)
                      (compile-ir e2 env)
                      (list '(cmp))
                      (list '(equal?))))
             
             
             (,lit when (constant? lit)
                   (begin
                     (set! fs (+ fs 1))
                     (list `(push_lit ,lit))))

             (,var when (variable? var)
                   (begin
                     (let* ((var-val (assoc var env))
                            (ir-code
                             (if var-val
                                 `((push_loc ,(- (+ fs (cdr var-val)) 1)))
                                 `((push_glo ,(env-lookup env-ir var))))))
                       (set! fs (+ fs 1))
                       ir-code)))
             
             ((,E0 . ,Es)
              (let ((old-fs fs) (nargs (length Es)))
                (begin
                  (set! fs nargs)
                  (append (compile-ir-bloc Es env)
                          (compile-ir E0 env)
                          (list `(call ,nargs)))))))))


(define lambda-count 0)
(define label-count 0)
(define label-gensym
  (lambda ()
    (set! label-count (+ label-count 1))
    (list "lab_" (number->string (- label-count 1)))))

;; generation lambda symbol
(define lambda-gensym 
  (lambda ()
    (begin
      (set! lambda-count (+ lambda-count 1))
      (string-append "lam" (number->string (- lambda-count 1))))))

(trace compile-ir)
