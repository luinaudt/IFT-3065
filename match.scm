;;; Fichier: match.scm

;;; Ce fichier défini la macro match qui permet de faire du pattern-matching
;;; sur des données Scheme.

;;;----------------------------------------------------------------------------

;; Macros pour faire des évaluations pendant la phase d'expansion de
;; macro, et la phase d'exécution.

(define-macro (for-macro-expansion . body)
  (eval `(begin ,@body))
  #f)

(define-macro (for-macro-expansion-and-execution . body)
  `(begin
     (for-macro-expansion ,@body)
     ,@body))

;;;----------------------------------------------------------------------------

;; La macro "match".

(for-macro-expansion-and-execution

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

  (define gensym ;; une version de gensym utile pour le deboguage
    (let ((count 0))
      (lambda ()
        (set! count (+ count 1))
        (string->symbol (string-append "g" (number->string count))))))

)

;;;----------------------------------------------------------------------------

;;; Tests

(define (test-match)

  (define (expand1 expr)
    (match expr

      ((and)
       #t)
      ((and ,E1)
       E1)
      ((and ,E1 . ,reste)
       (let ((v (gensym)))
         `(let ((,v ,E1)) (if ,v (and . ,reste) ,v))))

      ((or)
       #f)
      ((or ,E1)
       E1)
      ((or ,E1 . ,reste)
       (let ((v (gensym)))
         `(let ((,v ,E1)) (if ,v ,v (or . ,reste)))))

      (,_
       (error "l'expression n'est ni un and ni un or"))))

  (define (expand2 expr)
    (match expr

      ((and)
       #t)
      ((and ,E1)
       (expand2 E1))
      ((and ,E1 . ,reste)
       (let ((v (gensym)))
         (expand2 `(let ((,v ,E1)) (if ,v (and . ,reste) ,v)))))

      ((or)
       #f)
      ((or ,E1)
       (expand2 E1))
      ((or ,E1 . ,reste)
       (let ((v (gensym)))
         (expand2 `(let ((,v ,E1)) (if ,v ,v (or . ,reste))))))

      ((if ,E1 ,E2 ,E3)
       `(if ,(expand2 E1)
            ,(expand2 E2)
            ,(expand2 E3)))

      ((let ,bindings ,body)
       (expand2 `((lambda ,(map car bindings) ,body)
                ,@(map cadr bindings))))

      ((lambda ,params ,body)
       `(lambda ,params ,(expand2 body)))

      ((,E1 . ,reste)
       (map expand2 (cons E1 reste)))

      (,E
       E)))

  (pp (expand1 '(and)))
  (pp (expand1 '(and (= x 42))))
  (pp (expand1 '(and (= x 42) (= y 55))))
  (pp (expand1 '(and (= x 42) (= y 55) (= z 99))))

  (pp (expand2 '(and)))
  (pp (expand2 '(and (= x 42))))
  (pp (expand2 '(and (= x 42) (= y 55))))
  (pp (expand2 '(and (= x 42) (= y 55) (= z 99))))

  ;; imprime:
  ;;
  ;; #t
  ;; (= x 42)
  ;; (let ((g1 (= x 42))) (if g1 (and (= y 55)) g1))
  ;; (let ((g2 (= x 42))) (if g2 (and (= y 55) (= z 99)) g2))
  ;; #t
  ;; (= x 42)
  ;; ((lambda (g3) (if g3 (= y 55) g3)) (= x 42))
  ;; ((lambda (g4) (if g4 ((lambda (g5) (if g5 (= z 99) g5)) (= y 55)) g4)) (= x 42))

)

;;(test-match)

;;;----------------------------------------------------------------------------
