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
(define genv '())
(define gcnt 0)
(define fs 0)


;; Called only a single time, by compiler.scm.
;; Makes sure stack is empty at the end.
(define (compile-ir-program exprs env)
  (if (null? exprs)
      '()
      (append (compile-ir (car exprs) env)
              (begin
                (set! fs 0)
                (list `(check-stack-integrity 0)))
              (compile-ir-program (cdr exprs) env))))


;; Called whenever a bloc of code is being compiled (lets and lambda).
;; Makes sure the result of each expression is being consumed and returns
;; wit a single value on the stack.
(define (compile-ir-body exprs env)
  (cond ((null? exprs)
         (error "empty body"))
        ((null? (cdr exprs))
         (compile-ir (car exprs) env))
        (else
         (append (compile-ir (car exprs) env)
                 (compile-ir-body (cdr exprs) env)))))


(define (compile-ir expr env)
  ;;(pp expr)
  ;;(pp env)
  ;;(pp fs)
  ;;(pp expr)
  (pp expr)
  (pp fs)
  (if (null? expr)
      '()
      (match expr
             
	     ((define ,var ,ex)
	      (let* ((var-val (assoc var genv))
                     (ir-code
                      (if var-val
                          (begin
                            (append (list `(comment ("re-def " ,var)))
                                    (compile-ir ex env)
                                    (list `(pop_glo ,(cdr var-val)))))
                          (begin
                            (set! genv (cons (cons var gcnt) genv))
                            (set! gcnt (+ gcnt 1))
                            (append (compile-ir ex env)
                                    (list `(comment ("def " ,var)))
                                    (list `(pop_glo ,(- gcnt 1))))))))
                (set! fs (- fs 1))
                ir-code))
	     
             ((lambda ,params . ,body)
              (let* ((proc-name (lambda-gensym))
                     (nb-params (length params))
                     (ref-ids (range 0 (- nb-params 1)))
                     (proc-env (append (map cons params ref-ids) env))
                     (old-fs fs))
                ;; generate ir-code for lambda-expression
                (set! fs (+ nb-params 1))  ;; params + return address
                (set! lambda-env (append (list `(comment ("lambda " ,proc-name ,(number->string fs))))
                                         (list `(proc ,proc-name ,nb-params))
                                         (compile-ir-body body proc-env)
                                         (list `(ret ,(- fs (+ nb-params 1))))
                                         lambda-env))
                ;; generate ir-code to push lambda-expression address
                (set! fs (+ old-fs 1))
                (list `(comment ("proc " ,proc-name))
                      `(push_proc ,proc-name))))

             ((let ,bindings . ,body)
              (let* ((vars (map car bindings))
                     (vals (map cadr bindings))
                     (ref-ids (range (+ fs 1) (+ fs (length vars))))
                     (extended-env (append (map cons vars ref-ids) env))
                     (target-fs (+ fs 1)))
                ;; push vals on stack and compile body with extended environment
                (append (list `(comment "let"))
                        (push-on-stack-ir vals env)
                        (compile-ir-body body extended-env)
                        (begin
                          (set! fs target-fs)
                          (list `(check-stack-integrity ,target-fs))))))

             ((make-closure ,code . ,fv)
              (let* ((fv-cnt (length fv))
                     (ir-code
                      (append (compile-ir code env)
                              (list `(comment "make closure"))
                              (list `(close ,fv-cnt)))))
                (set! fs (- fs fv-cnt))
                ir-code))

             ((closure-code $clo)
              (let ((ir-code
                     (list `(comment "proc-code ptr")
                           `(push_loc ,(- fs (cdr (assoc '$clo env)))))))
                (set! fs (+ fs 1))
                ir-code))

             ((closure-ref $this ,pos)
              (begin
                (set! fs (+ fs 1))
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
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(add)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($- ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(sub)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($* ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(mul)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
	     
             (($quotient ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(quotient)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($modulo ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(modulo)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             
             (($= ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(cmp))
			     (list '(equal?)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($< ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(cmp))
			     (list '(less?)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
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
                   (let* ((var-pos (assoc var env)))
                     (set! fs (+ fs 1))
                     (if var-pos
                         (append (list `(push_loc ,(- (- fs 1) (cdr var-pos))))
				 (list	`(comment ,(string-append (number->string fs) " pushloc"))))
                         (list `(push_glo ,(env-lookup genv var))))))
             
             ((,E0 . ,Es)
              (let* ((nb-params (length Es))
                     (ir-code
                      (append (list `(comment ,expr))
                              (list `(comment "the arguments"))
                              (push-on-stack-ir Es env)
                              (compile-ir E0 env)
                              (list `(comment "the call"))
                              (list `(call ,nb-params)))))
                (set! fs (- fs nb-params))
                ir-code)))))


(define lambda-count 0)
(define label-count 0)
(define label-gensym
  (lambda ()
    (set! label-count (+ label-count 1))
    (list "label_" (- label-count 1))))

;; generation lambda symbol
(define lambda-gensym 
  (lambda ()
    (begin
      (set! lambda-count (+ lambda-count 1))
      (string-append "lam_" (number->string (- lambda-count 1))))))

(define push-on-stack-ir
  (lambda (exprs env)
    (if (null? exprs)
        '()
        (append (compile-ir (car exprs) env)
                (push-on-stack-ir (cdr exprs) env)))))

(define range
  (lambda (start end)
    (if (> start end)
        '()
        (cons start (range (+ start 1) end)))))
;;(trace compile-ir)
;;(trace compile-ir-program)
