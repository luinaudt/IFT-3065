(include "match.scm")

(define (desugar expr)

  (define (insert-begin exprs acc)
    (let ((first (car exprs))
          (rest (cdr exprs)))
      (if (and (pair? first)
               (eq? (car first) 'define))
          (insert-begin rest (cons first acc))
          (append (reverse acc) `((begin ,@exprs))))))
  
  (match expr
         ;; define
         ((define ,f-params . ,body) when (list? f-params)
          `(define ,(car f-params)
             ,(desugar
               `(lambda ,(cdr f-params)
                  ,@body))))

         ;; lambda
         ((lambda ,params . ,body)
          (if (> (length body) 1)
              `(lambda ,params
                 ,@(insert-begin (map desugar body) '()))
              `(lambda ,params
                 ,@body)))
         
         ;; lists and pairs
         ((quote ,lit)
          `(quote ,lit))
         
         ;; constants and symbols
         (,c when (or (constant? c) (symbol? c))
             c)
         
         ;; procedure call
         ((,E0 . ,Es)
          `(,(desugar E0) ,@(map desugar Es)))
         
         ;; anything else
         (,_
          (error "unknown expression"))))



(define (simple-cps expr)
  (match expr
         (,const when (constant? const)
                 const)
         (,var when (variable? var)
               var)
	 
         ((quote ,c)
          `(quote ,c));;expr)
         ((set! ,var ,E1)
          `(set! ,var ,(simple-cps E1)))
         ((,op . ,Es) when (primitive? op)
          `(,op ,@(map simple-cps Es)))
	 ((lambda ,params ,Es)
          (let ((k (gensym)))
	    `(lambda (,k ,@params) ,(cps Es k))))
	 ;;	 ))
	 ((lambda ,params . ,Es)
	  (let ((k (gensym)))
	    `(lambda (,k ,@params) ,(cps `(begin ,@Es) k))))))

(define (multi-cps exprs body)
  (define (inner sexpr)
    (multi-cps (cdr exprs)
               (lambda (sexprs)
                 (body (cons sexpr sexprs)))))
  (if (null? exprs)
      (body `())
      (let ((expr (car exprs)))
        (if (simple? expr)
            (inner (simple-cps expr))
            (let ((r (gensym)))
              (cps expr
                   `(lambda (,r)
                      ,(inner r))))))))

(define (simple? expr)
  (match expr
         (,const when (constant? const)
                 #t)
         (,var when (variable? var)
               #t)
         ((quote ,c)
          #t)
	 
         ((lambda ,params . ,E0)
          #t)
	 
	 ((set! ,var ,E1)
          (simple? E1))
         ((,op . ,Es) when (primitive? op)
          (all simple? Es))
         (,_
          #f)))

(define (all pred lst)
  (or (null? lst)
      (and (pred (car lst)) (all pred (cdr lst)))))

(define (cps E K)
  (match E
         (,s when (simple? s)
             `(,K ,(simple-cps s)))
         ((set! ,v ,E1)
          (multi-cps (list E1)
                     (lambda (sexprs)
                       `(,K (set! ,v ,@sexprs)))))
         ((define ,v ,E1)
	  `(define ,v ,(cps E1 K)))

	 ((if ,E1 ,E2 ,E3)
          (multi-cps (list E1)
                     (lambda (sexprs)
                       `(if ,@sexprs
                            ,(cps E2 K)
                            ,(cps E3 K)))))
	 ((if ,E1 ,E2)
	  (multi-cps (list E1)
                     (lambda (sexprs)
                       `(if ,@sexprs
                            ,(cps E2 K)
			    ,(cps '(quote ()) K)))))
         ((begin ,E1 . ,Es)
          (if (null? Es)
              (cps E1 K)
              (let ((r1 (gensym)))
                (cps E1 `(lambda (,r1)
                           ,(cps `(begin ,@Es) K))))))
	 
	 ((,op . ,Es) when (primitive? op)
          (multi-cps Es
                     (lambda (sexprs)
                       `(,K (,op ,@sexprs)))))
	
         ((,E0 . ,Es)
          (multi-cps (cons E0 Es)
                     (lambda (sexprs)
                       `(,(car sexprs)
                         ,K
                         ,@(cdr sexprs)))))
         (,_
          (error "unknown expression"))))

