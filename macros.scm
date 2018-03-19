(include "match.scm")

(for-macro-expansion-and-execution

 (define-macro (begin . exprs)
   (if (not (null? exprs))
       (if (null? (cdr exprs))
	   (car exprs)
	   `((lambda ()
	       ,(car exprs)
	       ,(cons 'begin
		      (cdr exprs)))))))
 

 (define-macro (cond . clauses)
   (match (cons 'cond clauses)
	  ((cond)
	   `#f)
	  ((cond (else ,e1 . ,es))
	   `(begin ,e1 ,@es))
	  ((cond (else . ,es) . ,rest)
	   (error "improper else clause"))
	  ((cond (,test) . ,rest)
	   `(or ,test (cond ,@rest)))
	  ((cond (,test => ,fn) . ,rest)
	   (let ((v (gensym)))
	     `(let ((,v ,test))
		(if ,v
		    (,fn ,v)
		    (cond ,@rest)))))
	  ((cond (,test => . ,es) . ,rest)
	   (error "improper => clause"))
	  ((cond (,test ,e1 . ,es) . ,rest)
	   `(if ,test
		(begin ,e1 ,@es)
		(cond ,@rest)))))

 (define-macro (or . exprs)
   (cond ((null? exprs)
	  '#f)
	 ((null? (cdr exprs))
	  (car exprs))
	 (else
	  (let ((v (gensym)))
	    `(let ((,v ,(car exprs)))
	       (if ,v ,v ,(cons 'or (cdr exprs))))))))

 (define-macro (and . exprs)
   (cond ((null? exprs)
	  '#t)
	 ((null? (cdr exprs))
	  (car exprs))
	 (else
	  `(if ,(car exprs)
	       ,(cons 'and (cdr exprs))
	       #f))))

 (define-macro (let bindings . body)
   (if (null? bindings)
       `((lambda ()
	   ,@body))
       (cons `(lambda ,(map car bindings)
		,@body)
	     (map cadr bindings))))

 (define-macro (let* bindings . body)
   (cond ((null? bindings)
	  `((lambda ()
	      ,@body)))
	 ((null? (cdr bindings))
	  `(let (,(car bindings))
	     ,@body))
	 (#t
	  `(let (,(car bindings))
	     (let* ,(cdr bindings)
	       ,@body))))))
