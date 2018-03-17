;;The intermediate code generation part
;;This part takes as input an AST
;;generate a standard IR

;;the ast is a scheme list
;;it should contain unique symbols.
(include "match.scm")

;; Intermediate code
;; Fonction pour génération de la représentation intermdiaire.
;; retourne une liste AST avec le langage intermédiaire
(define env-ir '())
(define taille-glob 0)

(define (compile-ir expr env)
  ;;(pp expr)
  (if (null? expr)
      '()
      (match expr
	     ((define ,name ,expr)
	      (begin (set! env-ir (env-extend env-ir (list name) (list taille-glob)))
		     (set! taille-glob (+ taille-glob 1))
		     (append (compile-ir expr env)
			     (list `(pop_glo ,(- taille-glob 1))))))
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
		   (list `(push_glo ,(env-lookup env-ir var)))) ;;(list var))))
	     ((,E0 . ,Es)
	      (append (compile-ir E0 env)
		      (compile-ir Es env))))))



;;debug
;;(pp compile-ir)
;;(trace compile-ir)
;;(trace intermediateCode-gen)
;;(trace ir-analyse-println)
;;(trace ir-analyse-add)
