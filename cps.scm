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
          expr)
         ((set! ,var ,E1)
          `(set! ,var ,(simple-cps E1)))
         ((,op . ,Es) when (primitive? op)
          `(,op ,@(map simple-cps Es)))
         ((lambda ,params . ,Es)
          (let ((k (gensym)))
            `(lambda (,k ,@params) ,@(map (lambda (e) (cps e k)) Es))))))

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

;; (trace desugar)
;; (trace cps)
;; (trace simple-cps)
;; (trace multi-cps)

;; (define (cps E K)
;;   (match E
;;          (,c when (or (number? c) (string? c) (boolean? c) (null? c) (char? c))
;;              `(,K ,c))
;;          (,v when (symbol? v)
;;              `(,K ,v))
;;          ((set! ,v ,E1)
;;           (let ((r1 (gensym)))
;;             (cps E1 `(lambda (,r1) (,K (set! ,v ,r1))))))
;;          ((if ,E1 ,E2 ,E3)
;;           (let ((r1 (gensym)))
;;             (cps E1 `(lambda (,r1)
;;                        (if ,r1 ,(cps E2 K) ,(cps E3 K))))))
;;          ((begin ,E1 ,E2)
;;           (let ((r1 (gensym)))
;;             (cps E1 `(lambda (,r1) ,(cps E2 K)))))
;;          ((,op ,E1 ,E2) when (primitive? op)
;;           (let ((r1 (gensym)) (r2 (gensym)))
;;             (cps E1 `(lambda (,r1)
;;                        ,(cps E2 `(lambda (,r2)
;;                                    (,K (,op ,r1 ,r2))))))))
;;          ((lambda ,params ,E0)
;;           (let ((k (gensym)))
;;             `(,K (lambda (,k ,@params) ,(cps E0 k)))))
;;          ((,E0)
;;           (let ((r0 (gensym)))
;;             (cps E0 `(lambda (,r0) (,r0 ,K)))))
;;          ((,E0 ,E1)
;;           (let ((r0 (gensym)) (r1 (gensym)))
;;             (cps E0 `(lambda (,r0)
;;                        ,(cps E1 `(lambda (,r1)
;;                                    (,r0 ,K ,r1)))))))
;;          ((,E0 ,E1 ,E2)
;;           (let ((r0 (gensym)) (r1 (gensym)) (r2 (gensym)))
;;             (cps E0 `(lambda (,r0)
;;                        ,(cps E1 `(lambda (,r1)
;;                                    ,(cps E2 `(lambda (,r2)
;;                                                (,r0 ,K ,r1 ,r2)))))))))
;;          ((,E0 ,E1 ,E2 ,E3)
;;           (let ((r0 (gensym)) (r1 (gensym)) (r2 (gensym)) (r3 (gensym)))
;;             (cps E0 `(lambda (,r0)
;;                        ,(cps E1 `(lambda (,r1)
;;                                    ,(cps E2 `(lambda (,r2)
;;                                                ,(cps E3 `(lambda (,r3)
;;                                                            (,r0 ,K ,r1 ,r2 ,r3)))))))))))
;;          (,_
;;           (error "unknown expression"))))

;; (pp (cps '(+ 5 x)
;;          '(lambda (r) (halt r))))

;; (define (test expr cont)
;;   (pp `(cps ',expr ',cont))
;;   (pp '=>)
;;   (pp (cps expr cont))
;;   (newline))

;; (begin (test '(write (+ 1 2))
;;              '(lambda (r) (halt r))))

;; (begin (test '(lambda (x) (+ x x))
;;              '(lambda (r) (halt r))))

;; (begin (test '(set! append
;;                     (lambda (lst1 lst2)
;;                       (if (null? lst1)
;;                           lst2
;;                           (cons (car lst1)
;;                                 (append (cdr lst1) lst2)))))
;;              '(lambda (r) (halt r))))

;; (begin (test '(set! somme
;;                     (lambda (n lst)
;;                       (if (null? lst)
;;                           n
;;                           (somme (+ n (car lst))
;;                                  (cdr lst)))))
;;              '(lambda (r) (halt r))))
