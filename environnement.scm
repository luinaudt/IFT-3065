;; fichier pour la gestion de l'environnement.
(include "match.scm")
;;environnement de compilation
(define gcte prims)
;;environnement d'exécution
(define grte prims)

;;recherche dans l'enironnement
(define (env-lookup env var)
  (let ((x (assq var env)))
    (if x (cdr x) (error "unbound " var))))


;;extension de l'environnement pour les lambda
(define (cte-extend cte vars) (append vars cte))
(define (rte-extend rte vars) (append vars rte))


;;fonction de recherche
(define (cte-lookup cte var)
  (let ((x (memq var cte)))
    (if x
	(- (length cte) (length x))
	(error "unbound " var))))

(define (rte-lookup rte pos)
  (list-ref rte pos))

(define (constant? x)
  (or (number? x)
      (string? x)
      (boolean? x)))
(define (variable? x)
  (symbol? x))

;;fonction pour l'evaluation
(define (ev expr cte rte)
  (match expr
	 (,const when (constant? const)
		 const)

	 (,var when (variable? var)
	       (let ((pos (cte-lookup cte var)))
		 (rte-lookup rte pos)))

	 ((lambda ,params ,body)
	  (lambda args (ev body (cte-extend cte params)
			   (rte-extend rte args))))

	 ((,fun . ,exprs)
	  (apply (ev fun cte rte)
		 (map (lambda (x) (ev x cte rte))
		      exprs)))))

(define (eval expr)
  (ev expr gcte grte))

;;(trace eval)
;;(trace ev)
;;(trace rte-lookup)
;;(trace cte-lookup)
;;(trace env-lookup)
;;(trace cte-extend)
;;(trace rte-extend)
