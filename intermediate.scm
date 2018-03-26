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
		     (old-fs fs)
                     (len (length params))
                     (range (let loop ((x 0))
                              (if (< x len) 
                                  (cons x (loop (+ x 1)))
                                  '())))
                     (loc-env (map cons params (reverse range))))
		(begin
		  (pp body)
		  (set! fs (+ 1 len))
		  (set! lambda-env (append lambda-env
					   (append `((proc ,name  ,len))
						   (compile-ir-bloc body (append loc-env env))
						   `((ret 1)))))
		  (set! fs (+ 1 old-fs))
		  `((push_proc ,name)))))
	     ((if ,cond ,E0)
	      (let ((labend (label-gensym)))
		(append (compile-ir cond env)
			(compile-ir '#f '())
			(list '(cmp))
			(list `(jmpe ,labend))
			(compile-ir E0 env)
			(list `(lab ,labend)))))
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
			(list `(lab ,labend)))));;fin
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
		      (list '(quotient))))
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
		   (begin
		     (set! fs (+ 1 fs))
		     (list `(push_lit ,lit))))

	     (,var when (variable? var)
		   (begin
		      (set! fs (+ 1 fs))
		      (let ((var-val (assoc var env)))
			(if var-val
			    (begin
			      (pp var-val)
			      (pp fs)
			    `((push_loc ,(- fs (+ 1 (cdr var-val))))))
			    `((push_glo ,(env-lookup env-ir var))
			      )))))
	     
	     ((,E0 . ,Es)
	      (begin ;;(pp expr)
		;;(pp Es)
		(append (compile-ir-bloc Es env)
			(compile-ir E0 env)
			(list `(call ,(length Es)))))
	      ))))



;;debug
;;(pp compile-ir)
;;(trace compile-ir-bloc)
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
