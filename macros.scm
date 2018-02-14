(define-macro (begin . exprs)
  (if (not (null? exprs))
      (if (null? (cdr exprs))
          (car exprs)
          `((lambda ()
              ,(car exprs)
              ,(cons 'begin
                     (cdr exprs)))))))

(define-macro (cond . clauses)
  (if (not (null? clauses))
      `(if ,(caar clauses)
           (begin
             ,@(cdar clauses))
           ,(cons 'cond
                  (cdr clauses)))))

(define-macro (or . exprs)
  (cond ((null? exprs)
         '#f)
        ((null? (cdr exprs))
         (car exprs))
        (#t
         `(let ((first ,(car exprs)))
            (if first
                first
                ,(cons 'or (cdr exprs)))))))

(define-macro (and . exprs)
  (cond ((null? exprs)
         '#t)
        ((null? (cdr exprs))
         (car exprs))
        (#t
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
              ,@body)))))
