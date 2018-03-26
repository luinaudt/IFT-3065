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

	 ;; or
         
	 ((or)
	  #f)
	 ((or ,E1)
	  (expand-macros E1))
	 ((or ,E1 . ,reste)
	  (let ((v (gensym)))
	    (expand-macros `(let ((,v ,E1)) (if ,v ,v (or . ,reste))))))

	 ;; let
         
         ((let ,bindings . ,body) when (null? bindings)
          `((lambda ()
              ,@(expand-macros body))))
         
         ((let ,bindings . ,body) when (pair? bindings)
          (expand-macros
           `((lambda ,(map car bindings)
               ,@body)
             ,@(map cadr bindings))))
         
         ;; let nommé
         
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

	 ;; let*
         
	 ((let* ,bindings . ,body) when (null? bindings)
          `((lambda ()  
              ,@(expand-macros body))))
         
         ((let* ,bindings . ,body) when (and (pair? bindings) (null? (cdr bindings)))
          (expand-macros
           `(let (,(car bindings))
              ,@body)))
         
         ((let* ,bindings . ,body) when (pair? bindings)
          (expand-macros
           `(let (,(car bindings))
              (let* ,(cdr bindings)
                ,@body))))
	 
         ;; letrec
         
         ((letrec ,bindings . ,body) when (null? bindings)
          `((lambda ()
              ,@(expand-macros body))))

         ((letrec ,bindings . ,body) when (pair? bindings)
          (let ((syms (map (lambda (x) (gensym))  ;; generate a symbol for each variable
                           (map car bindings))))
            (begin
              (expand-macros
               `(let ,(map $instantiate (map car bindings))  ;; instantiate variables
                  (let ,(map list syms (map cadr bindings))  ;; evaluate expressions
                    ,@(map $set! (map car bindings) syms)    ;; assign them to variables
                    ,@body))))))
          
	 ;; begin
         
	 ((begin)
          #!void)
         ((begin ,E1)
          (expand-macros E1))
         ((begin ,E1 . ,reste)
          (let ((v (gensym)))
            (expand-macros
             `(let ((,v ,E1))
                (begin ,@reste)))))

         ;; cond
         
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
                (if ,v (,fn ,v) (cond ,@reste))))))
         ((cond (,test => . ,Es) . ,reste)
          (error "improper => clause"))
         ((cond (,test ,E1 . ,Es) . ,reste)
          (expand-macros
           `(if ,test
                (begin ,E1 ,@Es)
                (cond ,@reste))))

         ;; lists
         
	 ((,E0)
          (list (expand-macros E0)))
         ((,E0 . ,E1)
	  (cons (expand-macros E0)
		(expand-macros E1)))

         ;; anything else
         
	 (,e
	  e)))


;;(trace expand-macros)


(define ($instantiate x)
  (cons x (list #!unbound)))

(define ($set! var val)
  `(set! ,var ,val))
