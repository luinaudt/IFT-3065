;; File: "closure-conv.scm"

;; This file implements the steps of closure-conversion:
;;
;;   - alpha-conversion
;;   - assignment-conversion
;;   - closure-conversion

;; Alpha-conversion.

(include "match.scm")
(define (alpha-conv expr)
  (alphac expr '()))

(define (alphac expr env)

  (define (ac e)
    (alphac e env))

  (define (rename v)
    (cond ((assq v env) => cdr)
          (else v)))

  (match expr

         (,c when (constant? c)
             expr)

         ((quote ,x)
          expr)

         (,v when (variable? v)
             (rename v))

         ((set! ,v ,E1)
          `(set! ,(rename v) ,(ac E1)))

         ((define ,v ,E1)
          `(define ,(rename v) ,(ac E1)))

         ((lambda ,params . ,E) when (list? params)
          (let* ((fresh-params
                  (map (lambda (p) (cons p (gensym)))
                       params))
                 (new-env
                  (append fresh-params env)))
            `(lambda ,(map cdr fresh-params)
               ,@(map (lambda (e) (alphac e new-env)) E))))

         ((lambda ,params . ,E) when (pair? params)
          (let* ((fresh-params
                  (let map ((lst params))
                    (if (pair? lst)
                        (cons (cons (car lst) (gensym))
                              (map (cdr lst)))
                        (cons (cons lst (gensym)) '()))))
                 (new-env (append fresh-params env))
                 (new-params
                  (let map ((lst fresh-params))
                    (if (pair?  (cdr lst))
                        (cons (cdr (car lst)) (map (cdr lst)))
                        (cdr (car lst))))))
            `(lambda ,new-params
               ,@(map (lambda (e) (alphac e new-env)) E))))
         
         ((let ,bindings . ,E)
          (let* ((fresh-vars
                  (map (lambda (b) (cons (car b) (gensym)))
                       bindings))
                 (new-env
                  (append fresh-vars env)))
            `(let ,(map (lambda (v e) `(,(cdr v) ,(ac (cadr e))))
                        fresh-vars
                        bindings)
               ,@(map (lambda (e) (alphac e new-env)) E))))

         ((if ,E1 ,E2)
          `(if ,(ac E1) ,(ac E2)))
         ((if ,E1 ,E2 ,E3)
          `(if ,(ac E1) ,(ac E2) ,(ac E3)))

         ((,E0 . ,Es)
          `(,(if (primitive? E0) E0 (ac E0))
            ,@(map ac Es)))

         (,_
          (error "unknown expression" expr))))

;;;----------------------------------------------------------------------------

;; Assignment-conversion.

(define (assign-conv expr)
  (let ((globals (fv expr)))
    (assignc expr (difference (mv expr) globals))))

(define (assignc expr mut-vars)

  (define (ac e)
    (assignc e mut-vars))

  (define (mutable? v)
    (memq v mut-vars))

  (match expr

         (,c when (constant? c)
             expr)

         ((quote ,x)
          expr)

         (,v when (variable? v)
             (if (mutable? v) `(car ,v) v))

         ((set! ,v ,E1)
          (if (mutable? v)
              `(set-car! ,v ,(ac E1))
              `(set! ,v ,(ac E1))))

         ((define ,v ,E1)
          `(define ,v ,(ac E1)))
         
         
         ((lambda ,params . ,E) when (list? params)
          (let* ((mut-params
                  (map (lambda (p) (cons p (gensym)))
                       (keep mutable? params)))
                 (params2
                  (map (lambda (p)
                         (if (mutable? p)
                             (cdr (assq p mut-params))
                             p))
                       params)))
            `(lambda ,params2
               ,(if (null? mut-params)
                    `(let () ,@(map ac E))
                    `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) '())))
                                mut-params)
                       ,@(map ac E))))))
         
         ((lambda ,params-p . ,E) when (pair? params-p)
          (let* ((params
                  (let map ((lst params-p))
                    (if (pair? lst)
                        (cons (car lst) (map (cdr lst)))
                        (cons lst '()))))
                 (mut-params
                  (map (lambda (p) (cons p (gensym)))
                       (keep mutable? params)))
                 (params2
                  (map (lambda (p)
                         (if (mutable? p)
                             (cdr (assq p mut-params))
                             p))
                       params))
                 (new-params
                  (let map ((lst params2))
                    (if (pair?  (cdr lst))
                        (cons (car lst) (map (cdr lst)))
                        (car lst)))))
            `(lambda ,new-params
               ,(if (null? mut-params)
                    `(let () ,@(map ac E))
                    `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) '())))
                                mut-params)
                       ,@(map ac E))))))

         ((let ,bindings . ,E)
          (let* ((vars
                  (map car bindings))
                 (mut-vars
                  (map (lambda (v) (cons v (gensym)))
                       (keep mutable? vars)))
                 (vars2
                  (map (lambda (v)
                         (if (mutable? v)
                             (cdr (assq v mut-vars))
                             v))
                       vars)))
            `(let ,(map (lambda (v e) `(,v ,(ac (cadr e))))
                        vars2
                        bindings)
               ,(if (null? mut-vars)
                    `(let () ,@(map ac E))
                    `(let ,(map (lambda (x) `(,(car x) (cons ,(cdr x) '())))
                                mut-vars)
                       ,@(map ac E))))))

         ((if ,E1 ,E2)
          `(if ,(ac E1) ,(ac E2)))
         ((if ,E1 ,E2 ,E3)
          `(if ,(ac E1) ,(ac E2) ,(ac E3)))

         ((,E0 . ,Es)
          `(,(if (primitive? E0) E0 (ac E0))
            ,@(map ac Es)))

         (,_
          (error "unknown expression" expr))))

;;;----------------------------------------------------------------------------

;; Closure-conversion.

(define (closure-conv expr)
  (let ((globals (fv expr)))
    (closurec expr '() globals)))

(define (closurec expr cenv globals)

  (define (cc e)
    (closurec e cenv globals))

  (define (pos id)
    (let ((x (memq id cenv)))
      (and x
           (- (length cenv)
              (length x)))))

  (match expr

         (,c when (constant? c)
             expr)

         ((quote ,x)
          expr)

         (,v when (variable? v)
             (let ((p (pos v)))
               (if p
                   `(closure-ref $this ,p)
                   v)))

         ((set! ,v ,E1)
          `(set! ,v ,(cc E1)))

         ((define ,v ,E1)
          `(define ,v ,(cc E1)))

         ((lambda ,params . ,E)
          (let ((new-cenv (difference (fv expr) globals)))
            `(make-closure
              (lambda ($this ,@params)
                ,@(map (lambda (e) (closurec e new-cenv globals)) E))
              ,@(map cc new-cenv))))

         ((let ,bindings . ,E)
          `(let ,(map (lambda (b) `(,(car b) ,(cc (cadr b)))) bindings)
             ,@(map cc E)))

         ((if ,E1 ,E2)
          `(if ,(cc E1) ,(cc E2)))
         ((if ,E1 ,E2 ,E3)
          `(if ,(cc E1) ,(cc E2) ,(cc E3)))

         ((,E0 . ,Es)
          (if (primitive? E0)
              `(,E0 ,@(map cc Es))
              `(let (($clo ,(cc E0)))
                 ((closure-code $clo)
                  $clo
                  ,@(map cc Es)))))

         (,_
          (error "unknown expression" expr))))

;;;----------------------------------------------------------------------------

;; Free-variable and mutable-variable analyses.

(define (fv expr)
  (match expr

         (,c when (constant? c)
             '())

         ((quote ,x)
          `())

         (,v when (variable? v)
             (list v))

         ((set! ,v ,E1)
          (union (list v) (fv E1)))

         ((define ,v ,E1)
          (union (list v) (fv E1)))

         ((lambda ,params . ,E) when (list? params)
          (difference `,@(map fv E) params))
         
         ((lambda ,params . ,E) when (pair? params)
          (difference `,@(map fv E)
                      (let map ((lst params))
                        (if (pair? lst)
                            (cons (cons (car lst) (gensym))
                                  (map (cdr lst)))
                            (cons (cons lst (gensym)) '())))))

         ((let ,bindings . ,E)
          (union (apply union (map (lambda (b) (fv (cadr b))) bindings))
                 (difference (apply union (map fv E)) (map car bindings))))

         ((if ,E1 ,E2)
          (union (fv E1) (fv E2)))
         ((if ,E1 ,E2 ,E3)
          (union (fv E1) (fv E2) (fv E3)))

         ((,E0 . ,Es)
          (union (if (primitive? E0)
                     '()
                     (fv E0))
                 (apply union (map fv Es))))

         (,_
          (error "unknown expression" expr))))

(define (mv expr)
  (match expr

         (,c when (constant? c)
             '())

         ((quote ,x)
          `())

         (,v when (variable? v)
             '())

         ((set! ,v ,E1)
          (union (list v) (mv E1)))

         ((define ,v ,E1)
          (union (list v) (mv E1)))

         ((lambda ,params ,E)
          (mv E))

         ((let ,bindings ,E)
          (union (apply union (map (lambda (b) (mv (cadr b))) bindings))
                 (mv E)))

         ((if ,E1 ,E2)
          (union (mv E1) (mv E2)))
         ((if ,E1 ,E2 ,E3)
          (union (mv E1) (mv E2) (mv E3)))

         ((,E0 . ,Es)
          (union (if (primitive? E0) '() (mv E0))
                 (apply union (map mv Es))))

         (,_
          (error "unknown expression" expr))))

;;;----------------------------------------------------------------------------

;; Set operations.

(define (union . ss)
  (let loop ((lst ss) (result '()))
    (if (null? lst)
        result
        (loop (cdr lst)
              (union2 result (car lst))))))

(define (union2 s1 s2)
  (cond ((null? s1)
         s2)
        ((member (car s1) s2)
         (union2 (cdr s1) s2))
        (else
         (cons (car s1)
               (union2 (cdr s1) s2)))))

(define (intersect s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (cons (car s1)
               (intersect (cdr s1) s2)))
        (else
         (intersect (cdr s1) s2))))

(define (difference s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (difference (cdr s1) s2))
        (else
         (cons (car s1)
               (difference (cdr s1) s2)))))

(define (set-equal? s1 s2)
  (and (null? (difference s1 s2))
       (null? (difference s2 s1))))

(define (keep f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))

;;;----------------------------------------------------------------------------
;;(trace alpha-conv)
;;(trace alphac)
;;(trace assign-conv)
;;(trace assignc)
;;(trace closure-conv)
;;(trace closurec)
(trace fv)
;;(trace mv)
