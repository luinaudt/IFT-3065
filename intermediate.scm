;;The intermediate code generation part
;;This part takes as input an AST
;;generate a standard IR

;;the ast is a scheme list
;;it should contain unique symbols.
(include "match.scm")
;; Intermediate code
;; Fonction pour génération de la représentation intermdiaire.
;; retourne une liste AST avec le langage intermédiaire
;; environnement : environnement cte rte
(define (intermediateCode-gen expr cte rte)
  ;;  (pp (cdr expr))
  (match expr
	 (,null when (null? expr)
		'())
	 ((define ,name ,exprs)
	  (begin		   
	    (set! grte (rte-extend rte (intermediateCode-gen exprs cte rte)))
	    (set! gcte (cte-extend cte (list name))) ))
	 ((set! ,name ,exprs)
	  (error "set! not supported"))
	 ((let ,liste ,exprs)
	  (error "let not supported"))
	 ((quote ,exprs)
	  (error "quote not supported yet"))
	 ((lambda ,params ,body)
	  (intermediateCode-gen body
				(cte-extend cte params)
				(rte-extend rte params)))
	 ;;		 (error "lambda not supported yet"))
	 
	 (,number when (number? expr)
		  number)
	 (,var when (variable? expr)
	       (rte-lookp rte (cte-lookup cte var)))
	 ((,fun . ,exprs)
	  (if (pair? fun)
	      (let* ((ret (intermediateCode-gen fun cte rte)))
		(cons ret (intermediateCode-gen exprs gcte grte)))
	      (cons (rte-lookup rte (cte-lookup cte fun))
		    (intermediateCode-gen exprs cte rte))))))

;;implementation de la fonction $prinln
(define (analyse-println expr cte rte);; gcte grte)
  (match expr
   (,number when (number? expr)
	    (list 'println expr))
   (,pair when (pair? expr)
	  (list 'println (intermediateCode-gen expr cte rte)))))

(define (analyse-add expr1 expr2 cte rte)
  (list '$+ (intermediateCode-gen expr1 cte rte) (intermediateCode-gen expr2 cte rte)))

;; (pp intermediateCode-gen)
 (trace intermediateCode-gen)
 (trace analyse-println)
(trace analyse-add)
