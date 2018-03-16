;;The intermediate code generation part
;;This part takes as input an AST
;;generate a standard IR

;;the ast is a scheme list
;;it should contain unique symbols.
(include "match.scm")
;; Intermediate code
;; Fonction pour génération de la représentation intermdiaire.
;; retourne une liste AST avec le langage intermédiaire
(define (intermediateCode-gen expr cte rte)
  (pp cte)
;;  (pp expr)
   (match expr
  	 (,null when (null? expr)
  		'())
  	 ((define ,name ,exprs)
  	  (begin		   
  	    (set! grte (rte-extend rte (intermediateCode-gen exprs (cte-extend cte (list name)) rte)))
  	    (set! gcte (cte-extend cte (list name)))
  	    ))
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
	 ((if ,E1 ,E2)
	  expr)
	 ((if ,E1 ,E2 ,E3)
	  expr)
  	 (,number when (number? expr)
  		  number)
  	 (,var when (variable? expr)
  	       (rte-lookup rte (cte-lookup cte var)))
	 (,prim when (primitive? prim)
		(begin
		  (pp "opk")
		  prim))
	 ((make-closure . ,exprs)
	  (intermediateCode-gen exprs cte rte))
  	 ((,fun . ,exprs)
  	      (let* ((ret (intermediateCode-gen fun cte rte)))
  		(cons ret (intermediateCode-gen exprs cte rte))))))
;;implementation de la fonction $prinln
;;(define (make-closure expr cte rte)
;;  (match expr
;;	 ())
;;  )

(define (ir-analyse-println expr cte rte);; gcte grte)
  (match expr
   (,number when (number? expr)
	    (list 'println expr))
   (,pair when (pair? expr)
	  (list 'println (intermediateCode-gen expr cte rte)))))

(define (ir-analyse-add expr1 expr2 cte rte)
  (list '$+ (intermediateCode-gen expr1 cte rte) (intermediateCode-gen expr2 cte rte)))

(trace intermediateCode-gen)
;;(trace ir-analyse-println)
;;(trace ir-analyse-add)
