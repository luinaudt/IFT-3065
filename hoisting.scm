;;hoixting for closure
;;(define new-define '())
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
				      (set! new-define (cons (hoist-closing `(define ,sym ,code))
							     new-define))
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
    (append new-define new-code)))
;;(trace hoist-closure)
