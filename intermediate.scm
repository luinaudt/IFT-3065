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
                (set! lambda-env (env-extend lambda-env
                                             (list name)
                                             (list (append `((proc ,name))
                                                           (compile-ir body
                                                                       (append loc-env env))
                                                           `((ret 1))))))
                `((push_proc ,name))))
             
             
	     (($println  ,expr)
	      (append (compile-ir expr env)
		      (list '(println))))
             
	     (($+ ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(add))))
             
	     (,lit when (constant? lit)
		   (list `(push_lit ,lit)))
             
	     (,var when (variable? var)
                   (let ((var-val (assoc var env)))
                     (if var-val
                         `((push_loc ,(cdr var-val)))
                         `((push_glo ,(env-lookup (append env env-ir) var))))))
             
	     ((,E0 . ,Es)
	      (append (compile-ir E0 env)
		      (compile-ir Es env))))))



;;debug
;;(pp compile-ir)
(trace compile-ir)
;;(trace intermediateCode-gen)
;;(trace ir-analyse-println)
;;(trace ir-analyse-add)


(define lambda-count 0)

(define lambda-gensym ;; une version de gensym utile pour le deboguage
  (lambda ()
    (set! lambda-count (+ lambda-count 1))
    (string->symbol (string-append "lam" (number->string lambda-count)))))
