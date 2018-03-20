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


(define (compile-ir-bloc expr env)
  (let ((env-old env-ir))
    (if (null? expr)
	(begin
	  (set! env-ir env-old)
	  '())
	(append (compile-ir (car expr) env)
		(compile-ir-bloc (cdr expr) env)))))

(define (compile-ir expr env)
  ;;(pp expr)
  (if (null? expr)
      '()
      (match expr
            
	     ((define ,name ,expr)
	      (let ((var-val (assoc name env-ir)))
                (if var-val
                    (begin
                      (append (compile-ir expr env)
                              (list `(pop_glo ,(cdr var-val)))))
                    (begin
                      (set! env-ir (env-extend env-ir (list name) (list taille-glob)))
                      (set! taille-glob (+ taille-glob 1))
                      (append (compile-ir expr env)
                              (list `(pop_glo ,(- taille-glob 1))))))))
	     
             ((lambda ,params . ,body)
              (let* ((name (lambda-gensym))
                     (len (length params))
                     (range (let loop ((x 0))
                              (if (< x len) 
                                  (cons x (loop (+ x 1)))
                                  '())))
                     (loc-env (map cons params (reverse range))))
                (set! lambda-env (append lambda-env
					 (append `((proc ,name  ,(length params)))
						 (compile-ir-bloc body (append loc-env env))
						 `((ret 1)))))
                `((push_proc ,name))))
	     
             (($- ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(sub))))
	     (($println  ,expr)
	      (append (compile-ir expr env)
		      (list '(println))))
		      
             (($* ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(mul))))
	     (($modulo ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(modulo))))
	     (($quotient ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(mul))))
	     (($= ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(cmp))
		      (list '(equal?))))
	     (($< ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(cmp))
		      (list '(less?))))
	     (($+ ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(add))))
             
	     (,lit when (constant? lit)
		   (list `(push_lit ,lit)))

	     (,var when (variable? var)
                   (let ((var-val (assoc var env)))
                     (if var-val
                         `((push_loc ,(length env)))
			 ;;(error "unbound variable")
                         `((push_glo ,(env-lookup env-ir var))
			   ;;(call 2))
			   ))))
             
	     ((,E0 . ,Es)
	      (begin (pp E0)
		     (pp Es)
		     (append (compile-ir Es env)
		      (compile-ir E0 env)
		      (list `(call ,(length Es)))))
	     ))))
  
(define (compile-ir-call fn expr env)
  (error "")
  )

;;debug
;;(pp compile-ir)
;;(trace compile-ir)
;;(trace intermediateCode-gen)
;;(trace ir-analyse-println)
;;(trace ir-analyse-add)


(define lambda-count 0)
(define label-count 0)
(define label-gensym
  (lambda ()
    (set! label-count (+ label-count 1))
    (list "lab_" (number->string label-count))))

;; generation lambda symbol
(define lambda-gensym 
  (lambda ()
    (begin
      (set! lambda-count (+ lambda-count 1))
      (string-append "lam" (number->string lambda-count)))))
