(include "match.scm")

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
	 ((let ,name ,bindings . ,body) when (and (symbol? name) (null? bindings))
	  (expand-macros
           `((letrec ((,name (lambda ()
                               ,@body)))
               ,name))))
         
	 ((let ,name ,bindings . ,body) when (symbol? name)
	  (expand-macros
           `((letrec ((,name (lambda ,(map car bindings)
                               ,@body)))
               ,name)
             ,@(map cadr bindings))))

	 ;;let*
	 ((let* ,bindings . ,body)
	  (if (null? bindings)
	      (expand-macros `(lambda () ,@body))
	      (if (null? (cdr bindings))
		  (expand-macros `(let (,(car bindings)) ,@body))
		  (expand-macros `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))))))
	 
	 ;;begin
	 ((begin)
          #!void)
         ((begin ,E1)
          (expand-macros E1))
         ((begin ,E1 . ,reste)
          (let ((v (gensym)))
            (expand-macros
             `(let ((,v ,E1))
                (begin ,@reste)))))

	 ;;letrec
<<<<<<< HEAD
         

=======
	 
	 
	 ;;cond
>>>>>>> 58d933f7611ad344872e6efbea78b3a042fb0159
         ((cond)
          #f)
         ((cond (else ,E1 . ,Es))
          (expand-macros
           `(begin ,E1 ,@Es)))
         ((cond (else . ,Es) . ,reste)
          (error "improper else clause"))
         ((cond (,test) . ,reste)
          (expand-macros
           `(or ,test (cond ,@reste))))
         ((cond (,test => ,fn) . ,reste)
          (let ((v (gensym)))
            (expand-macros
             `(let ((,v ,test))
                (if ,v
                    (,fn ,v)
                    (cond ,@reste))))))
         ((cond (,test => . ,Es) . ,reste)
          (error "improper => clause"))
         ((cond (,test ,E1 . ,Es) . ,reste)
          (expand-macros
           `(if ,test
                (begin ,E1 ,@Es)
                (cond ,@reste))))

	 
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

;;(trace expand-macros)
