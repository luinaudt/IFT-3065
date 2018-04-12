(include "match.scm")
;;(include "aaa.scm")

(define genv '())

(define (ev expr env)
  (match expr
         
         (,const when (constant? const)
                 const)
         
         (,var when (variable? var)
               (env-lookup env var))

	 ((lambda ,params ,body)
	  (lambda args (ev body (env-extend env params args))))

         ((define ,symbol ,expr)
          (cons (cons symbol (ev expr env)) env))

         ((,fun . ,exprs)
          (apply (ev fun env)
                 (map (lambda (x) (ev x env))
                      exprs)))))

(define (env-lookup env var)
  (let ((x (assq var env)))
    (if x
        (cdr x)
        (error "unbound" var))))

(define (env-extend env vars vals)
  (append (map cons vars vals)
          env))

(define (constant? x)
  (or (number? x)
      (null? x)
      (equal? x #!void)
      (boolean? x)
      (char? x)
      (string? x)))

(define (variable? x)
  (symbol? x))

(define (eval expr)
  (ev expr genv))
