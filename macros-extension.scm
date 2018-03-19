
(define (expand-macros ast)
 
  (match ast
	 ;; and
	 ((and)
	  #t)
	 ((and ,E1)
	  (expand-macros E1))
	 ((and ,E1 . ,reste)
	  (let ((v (gensym)))
	    (expand-macros `(let ((,v ,E1)) (if ,v (and . ,reste) ,v)))))

	 ;;let
	 ((let ,bindings . ,body) when (pair? bindings)
	  (if (null? bindings)
	      (expand-macros `((lambda () ,@body)))
	      (expand-macros `((lambda ,(map car bindings) ,@body)
			   ,@(map cadr bindings)))))
	 ;;let nommé
	 ((let ,name ,bindings . ,body)
	  (error "k"))

	 ;;let*
	 ((let* ,bindings . ,body)
	  (if (null? bindings)
	      (expand-macros `(lambda () ,@body))
	      (if (null? (cdr bindings))
		  (expand-macros `(let (,(car bindings)) ,@body))
		  (expand-macros `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))))))
	 
	 ;;begin
	 
	 ;;letrec

	 ;;cond
	 
	 
	 ;;or
	 ((or)
	  #f)
	 ((or ,E1)
	  (expand-macros E1))
	 ((or ,E1 . ,reste)
	  (let ((v (gensym)))
	    (expand-macros `(let ((,v ,E1))
			      (if ,v ,v (or . ,reste))))))
	 ((,E0 . ,E1)
	  (cons (expand-macros E0)
		(expand-macros E1)))
	 (,e
	  e)))

(trace expand-macros)
;;(pp expand-macros)
