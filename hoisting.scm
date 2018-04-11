;;hoixting for closure
;;(define new-define '())

(define (hoist-closure-2 expr)
  (if (null? expr)
      '()
      (append (hoist-closure (car expr))
	    (hoist-closure-2 (cdr expr))))) 

(define (hoist-closure expr)
  (let* ((new-define '())
	 (new-code
	  (letrec ((hoist-closing
		    (lambda (expr) 
		      (if (null? expr)
			  '()
			  (match expr
				 ((make-closure ,code . ,fv)
				  (let ((sym (gensym)))
				    (begin
				      (let ((to-append (list (hoist-closing `(define ,sym ,code)))))
					(set! new-define (if (null? new-define)
							     to-append
							     (append new-define to-append)))) 
				      `(make-closure ,sym ,@fv))
				    
				    ;;		(map hoist-closure code)
				    ))
				 (,c when (constant? c)
				     expr)

				 ((quote ,x)
				  expr)

				 (,v when (variable? v)
				     expr)
				 ((,E0 . ,E1)
				  `(,E0 ,@(map hoist-closing E1))
				  ))))))
	    (hoist-closing expr))))
    (append new-define (list new-code))))
;;(trace hoist-closure)
;;(trace hoist-closure-2)
