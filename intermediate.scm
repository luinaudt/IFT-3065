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
  (if (null? expr)
      '()
      (match expr
	     ;;		((define ,expr)
	     ;;		 
	     ;;			 )
	     (,number when (number? expr)
		      expr)
	     (,pair when (pair? (car expr))
		    (cons (intermediateCode-gen (car expr))
			  (intermediateCode-gen (cdr expr))))
	     ((println ,expr)
	      (analyse-println expr))
	     ((+ ,expr1 ,expr2)
	      (analyse-add expr1 expr2)))))

;;implementation de la fonction $prinln
(define (analyse-println expr);; gcte grte)
  (match expr
   (,number when (number? expr)
	    (list 'println expr))
   (,pair when (pair? expr)
	  (list 'println (intermediateCode-gen expr)))))

(define (analyse-add expr1 expr2)
  (list '$+ (intermediateCode-gen expr1) (intermediateCode-gen expr2)))

;; (pp intermediateCode-gen)
;; (trace intermediateCode-gen)
;; (trace analyse-println)
