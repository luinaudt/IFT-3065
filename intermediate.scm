;;The intermediate code generation part
;;This part takes as input an AST
;;generate a standard IR

;;the ast is a scheme list
;;it should contain unique symbols.
(include "match.scm")
;; Intermediate code
;; Fonction pour génération de la représentation intermdiaire.
;; retourne une liste AST avec le langage intermédiaire
;; environnement : environnement gcte grte
(define (intermediateCode-gen expr);; gcte grte)
  (cond ((pair? (car expr))
	 (intermediateCode-gen (car expr)))
	((number? (car expr))
	 (car expr))
	((eq? (car expr) 'println)
	 (analyse-println (cdr expr)))
	(else (error "erreur unknown expression"))))
;;      (match expr
;;	     (($println expr)
;;	      (analyse-println ,expr))
;;
;;	     (($+ ,expr)
;;	      (analyse-add ,expr)))))

;;implementation de la fonction $prinln
(define (analyse-println expr);; gcte grte)
  (cond ((number? (car expr))
	 (list 'println (car expr)))))

;; (pp intermediateCode-gen)
;; (trace intermediateCode-gen)
;; (trace analyse-println)
