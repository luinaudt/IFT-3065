
(define-macro (match sujet . clauses)

  (define (if-equal? var gab oui non)
    (cond ((and (pair? gab)
                (eq? (car gab) 'unquote)
                (pair? (cdr gab))
                (null? (cddr gab)))
           `(let ((,(cadr gab) ,var))
              ,oui))
          ((null? gab)
           `(if (null? ,var) ,oui ,non))
          ((symbol? gab)
           `(if (eq? ,var ',gab) ,oui ,non))
          ((or (boolean? gab)
               (char? gab))
           `(if (eq? ,var ,gab) ,oui ,non))
          ((number? gab)
           `(if (eqv? ,var ,gab) ,oui ,non))
          ((pair? gab)
           (let ((carvar (gensym))
                 (cdrvar (gensym)))
             `(if (pair? ,var)
                  (let ((,carvar (car ,var)))
                    ,(if-equal?
                      carvar
                      (car gab)
                      `(let ((,cdrvar (cdr ,var)))
                         ,(if-equal?
                           cdrvar
                           (cdr gab)
                           oui
                           non))
                      non))
                  ,non)))
          (else
           (error "unknown pattern"))))

  (let* ((var
          (gensym))
         (fns
          (map (lambda (x) (gensym))
               clauses))
         (err
          (gensym)))
    `(let ((,var ,sujet))
       ,@(map (lambda (fn1 fn2 clause)
                `(define (,fn1)
                   ,(if-equal? var
                               (car clause)
                               (if (and (eq? (cadr clause) 'when)
                                        (pair? (cddr clause)))
                                   `(if ,(caddr clause)
                                        ,(cadddr clause)
                                        (,fn2))
                                   (cadr clause))
                               `(,fn2))))
              fns
              (append (cdr fns) (list err))
              clauses)
       (define (,err) (error "match failed"))
       (,(car fns)))))

(define count 0)

(define (gensym) (set! count (+ count 1)) (string->symbol (string-append "g" (number->string count))))

(define (cps E K)
  (match E
    (,c when (or (number? c) (string? c) (boolean? c))
     `(,K ,c))
    (,v when (symbol? v)
     `(,K ,v))
    ((set! ,v ,E1)
     (let ((r1 (gensym)))
       (cps E1 `(lambda (,r1) (,K (set! ,v ,r1))))))
    ((if ,E1 ,E2 ,E3)
     (let ((r1 (gensym)))
       (cps E1 `(lambda (,r1)
                  (if ,r1 ,(cps E2 K) ,(cps E3 K))))))
    ((begin ,E1 ,E2)
     (let ((r1 (gensym)))
       (cps E1 `(lambda (,r1) ,(cps E2 K)))))
    ((,op ,E1 ,E2) when (memq op '(+ - * / < <= =))
     (let ((r1 (gensym)) (r2 (gensym)))
       (cps E1 `(lambda (,r1)
                  ,(cps E2 `(lambda (,r2)
                              (,K (,op ,r1 ,r2))))))))
    ((lambda ,params ,E0)
     (let ((k (gensym)))
       `(,K (lambda (,k ,@params) ,(cps E0 k)))))
    ((,E0)
     (let ((r0 (gensym)))
       (cps E0 `(lambda (,r0) (,r0 ,K)))))
    ((,E0 ,E1)
     (let ((r0 (gensym)) (r1 (gensym)))
       (cps E0 `(lambda (,r0)
                  ,(cps E1 `(lambda (,r1)
                              (,r0 ,K ,r1)))))))
    ((,E0 ,E1 ,E2)
     (let ((r0 (gensym)) (r1 (gensym)) (r2 (gensym)))
       (cps E0 `(lambda (,r0)
                  ,(cps E1 `(lambda (,r1)
                              ,(cps E2 `(lambda (,r2)
                                          (,r0 ,K ,r1 ,r2)))))))))
    (,_
     (error "unknown expression"))))

#;
(pp (cps '(+ 5 x)
         '(lambda (r) (halt r))))

(define (test expr cont)
  (pp `(cps ',expr ',cont))
  (pp '=>)
  (pp (cps expr cont))
  (newline))

(begin (test '(write (+ 1 2))
         '(lambda (r) (halt r))))

(begin (test '(lambda (x) (+ x x))
         '(lambda (r) (halt r))))

(begin (test '(set! append
                (lambda (lst1 lst2)
                  (if (null? lst1)
                      lst2
                      (cons (car lst1)
                            (append (cdr lst1) lst2)))))
         '(lambda (r) (halt r))))

(begin (test '(set! somme
                (lambda (n lst)
                  (if (null? lst)
                      n
                      (somme (+ n (car lst))
                             (cdr lst)))))
         '(lambda (r) (halt r))))
