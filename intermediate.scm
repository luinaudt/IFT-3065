;;The intermediate code generation part
;;This part takes as input an AST
;;generate a standard IR

;;the ast is a scheme list
;;it should contain unique symbols.
(include "match.scm")

;; Intermediate code
;; Fonction pour génération de la représentation intermdiaire.
;; retourne une liste AST avec le langage intermédiaire

(define (compile-ir expr env)
  ;;(pp expr)
  (if (null? expr)
      '()
      (match expr
	     ((define ,name ,expr)
	      (append (compile-ir expr (env-extend env (list name) (list 0)))
		      (list `(pop_glo ,name))))
	     (($println  ,expr)
	      (append (compile-ir expr env)
		      (list '(println))))
	     (($+ ,p1 ,p2)
	      (append (compile-ir p1 env)
		      (compile-ir p2 env)
		      (list '(add))))
	     (,lit when (constant? lit)
		   (list (cons 'push_lit (list lit))))
	     (,var when (variable? var)
		   (list (cons 'push_glo (list var))))
	     ((,E0 . ,Es)
	      (append (compile-ir E0 env)
		      (compile-ir Es env))))))



;;debug
;;(pp intermediateCode-gen)
;;(trace compile-ir)
;;(trace intermediateCode-gen)
;;(trace ir-analyse-println)
;;(trace ir-analyse-add)
