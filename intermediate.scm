;;The intermediate code generation part
;;This part takes as input an AST
;;generate a standard IR

;;the ast is a scheme list
;;it should contain unique symbols.
(include "match.scm")

;; Intermediate code
;; Fonction pour génération de la représentation intermdiaire.
;; retourne une liste AST avec le langage intermédiaire
(include "environnement.scm")
(include "match.scm")
(include "genconst.scm")


(define lambda-env '())
(define genv '())
(define gcnt 0)
(define fs 0)

;; Called only by compiler.scm or recursively.
;; Ensures that the stack is emptied after each expression.
(define (compile-ir-program exprs env)

  (define (compile-ir-define expr env)
    (if (not (= (length expr) 3))
        (error "ill-formed special form: define")
        (let* ((var (cadr expr))
               (val (caddr expr))
               (var-pos (assoc var genv))
               (ir-code
                (if var-pos
                    (append (list `(comment ("re-def " ,var)))
                            (compile-ir val env)
                            (list `(pop_glo ,(cdr var-pos))))
                    (let ((name gcnt))
                      (begin
                        (set! genv (cons (cons var gcnt) genv))
                        (set! gcnt (+ gcnt 1))
                        (append (list `(comment ("def " ,var)))
                                (compile-ir val env)
                                (list `(pop_glo ,name))))))))
          (begin
            (set! fs (- fs 1))
            ir-code))))
  
  (cond ((null? exprs)
         '())
        ((and (pair? (car exprs))
              (eq? (car (car exprs)) 'define))
         (append (compile-ir-define (car exprs) env)
                 (compile-ir-program (cdr exprs) env)))
        (else
         (append (compile-ir (car exprs) env)
                 (begin
                   (set! fs 0)
                   (list `(check-stack-integrity 0)))
                 (compile-ir-program (cdr exprs) env)))))


;; Called whenever a bloc of code is being compiled (lets and lambdas).
;; Ensures that the result of each expression is being consumed and
;; that the body ends with a single value on the stack.
(define (compile-ir-body exprs env)

  (define body-defs '())
  
  (define (compile-ir-body-defs exprs env)
    (cond ((null? exprs)
           (error "body must contain at least one expr"))
          ((and (pair? (car exprs))
                (eq? (car (car exprs)) 'define))
           (append (compile-ir-define (car exprs) (append body-defs env))
                   (compile-ir-body-defs (cdr exprs) (append body-defs env))))
          (else
           (compile-ir-body-exprs exprs (append body-defs env)))))
  
  (define (compile-ir-define expr env)
    (if (not (= (length expr) 3))
        (error "ill-formed special form: define")
        (let* ((var (cadr expr))
               (val (caddr expr))
               (var-pos (assoc var env)))
          (cond ((assoc var body-defs)
                 (error "duplicate definition of a variable"))
                (var-pos
                 (begin
                   (set! body-defs (cons (cons var (cdr var-pos)) body-defs))
                   (append (list `(comment "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
                           (compile-ir val env)
                           (begin
                             ;;(set! fs (+ fs 1))
                             (list `(push_loc ,(- fs (cdr var-pos)))))
                           (begin
                             (set! fs (- fs 1))
                             (list `(set-car!))))))
                (else
                 (begin
                   (set! fs (+ fs 1))
                   (set! body-defs (cons (cons var fs) body-defs))
                   (append (list `(comment "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"))
                           (compile-ir val env))))))))
  
  (define (compile-ir-body-exprs exprs env)
    (if (null? (cdr exprs))
        (compile-ir (car exprs) env)
        (let* ((fs-save fs)
               (ir-code
                (append (compile-ir (car exprs) env)
                        (compile-ir-body-exprs (cdr exprs) env))))
          (begin
            (set! fs fs-save)
            (list `(check-stack-integrity ,fs-save))
            ir-code))))
  
  ;; (display fs)
  ;; (display "   ")
  ;; (if (not (null? exprs))
  ;;     (pp (car exprs)))

  
  ;; (trace compile-ir-body-defs)
  ;; (trace compile-ir-define)
  ;; (trace compile-ir-body-exprs)
  (begin
    (set! body-defs '())
    (if (null? exprs)
        (error "empty body")
        (compile-ir-body-defs exprs env))))




;; (trace compile-ir-body)
;; (trace compile-ir-program)



(define (compile-ir expr env)
  ;; (display fs)
  ;; (display "   ")
  ;; (pp expr)
  (if (null? expr)
      '()
      (match expr
             ((comment ,val)
	      (append (list expr)
		      (list '(lab "start"))))
             
	     ((define ,var ,ex)
              (error "ill-placed define"))
	     
             ((lambda ,params . ,body) when (list? params)
              (let* ((proc-name (lambda-gensym))
                     (nb-params (length params))
                     (ref-ids (range 0 (- nb-params 1)))
                     (proc-env (append (map cons params ref-ids) env))
                     (old-fs fs))
                (begin
                  ;; generate ir-code for lambda-expression
                  (set! fs (+ nb-params 1))  ;; params + return address
                  (set! lambda-env (append (list `(proc ,proc-name ,nb-params))
                                           (list `(comment ("lambda " ,proc-name " " ,fs)))
                                           (compile-ir-body body proc-env)
                                           (list `(ret ,(- fs (+ nb-params 1))))
                                           lambda-env))
                  ;; generate ir-code to push lambda-expression address
                  (set! fs (+ old-fs 1))
                  (list `(comment ("proc " ,proc-name))
                        `(push_proc ,proc-name)))))
             
	     ;;lambda avec parametre reste
	     ((lambda ,param-pair . ,body)
	      (let* ((proc-name (lambda-gensym))
		     (params
		      (let to-list ((lst param-pair))
			(if (pair? lst)
			    (cons (car lst) (to-list (cdr lst)))
			    (cons lst '()))))
		     (nb-params (length params))
		     (ref-ids (range 0 (- nb-params 1)))
                     (proc-env (append (map cons params ref-ids) env))
                     (old-fs fs))
                (begin
                  ;; generate ir-code for lambda-expression
                  (set! fs (+ nb-params 1))  ;; params + return address
                  (set! lambda-env (append (list `(proc_reste ,proc-name ,nb-params))
                                           (list `(comment ("lambda " ,proc-name " " ,fs)))
                                        ;					   ();;generer le parametre reste
                                           (compile-ir-body body proc-env)
                                           (list `(ret ,(- fs (+ nb-params 1))))
                                           lambda-env))
                  ;; generate ir-code to push lambda-expression address
                  (set! fs (+ old-fs 1))
                  (list `(comment ("proc reste " ,proc-name))
                        `(push_proc ,proc-name)))))
             
             ((let ,bindings . ,body)
              (let* ((vars (map car bindings))
                     (vals (map cadr bindings))
                     (ref-ids (range (+ fs 1) (+ fs (length vars))))
                     (extended-env (append (map cons vars ref-ids) env))
                     (target-fs (+ fs 1)))
                ;; push vals on stack and compile body with extended environment
                (append (list `(comment "let"))
			(push-on-stack-ir vals env)
                        (compile-ir-body body extended-env)
                        (begin
                          (set! fs target-fs)
                          (list `(check-stack-integrity ,target-fs))
			  ))))

             ((make-closure ,code . ,fv)
              (let* ((fv-cnt (length fv))
                     (ir-code
                      (append (compile-ir code env)
			      (push-on-stack-ir fv env)
			      (list `(push_lit ,(+ 3 fv-cnt))
				    `(alloc))
			      (list `(comment "make closure"))
			      (list `(close ,fv-cnt)))))
                (begin
                  (set! fs (- fs fv-cnt))
                  ir-code)))

             ((closure-code $clo)
              (let ((ir-code
                     (list `(comment ("*proc-code " ,fs))
                           `(push_loc ,(- fs (cdr (assoc '$clo env)))))))
                (begin
                  (set! fs (+ fs 1))
                  ir-code)))

             ((closure-ref $this ,pos)
              (begin
                (set! fs (+ fs 1))
                (list `(push_this ,fs)
                      `(push_free ,pos))))

             (($cons ,e1 ,e2)
              (let ((ir-code
                     (append (compile-ir e1 env)
                             (compile-ir e2 env)
                             (list '(push_lit 16)
			           '(alloc)
				   '(cons)))))
                (begin
                  (set! fs (- fs 1))
                  ir-code)))

             (($car ,p)
              (append (compile-ir p env)
                      (list '(car))))

             (($cdr ,p)
              (append (compile-ir p env)
                      (list '(cdr))))
             
	     ((set! ,v ,c)
              (if (not (variable? v))
                  (error "identifier expected")
                  (append (compile-ir c env)
                          (cond ((assoc v env)
                                 (list `(comment ("set local var, fs = " ,fs))
                                       `(pop_loc ,(cdr (assoc v env)))
                                       `(push_lit #!void)))
                                ((assoc v genv)
                                 (list `(comment ("set global var, fs = " ,fs))
                                       `(pop_glo ,(cdr (assoc v genv)))
                                       `(push_lit #!void)))
                                (else
                                 (error "unbound variable" v))))))
             
	     (($string-set! ,s ,pos ,c) 
	      (begin (set! fs (+ 0 fs))
		     (append ;;(list `(comment ,(string-append "string set " (string c))))
		      (compile-ir c env)
		      (compile-ir s env);;on récupère la position de str
		      (compile-ir pos env);;on calcul la position du caractère a modifier
		      (begin
			(set! fs (- fs 2))
			(list '(push_lit 1)
			      '(add)
			      '(push_tag 3)
			      '(sub)
			      '(add) ;;on détermine l'adresse (la représentation nous la garde
			      '(pop_mem)
                              `(push_lit #!void)))))) ;;on doit mettre dans le tas à la position
             (($set-car! ,p ,e)
              (let ((ir-code
                     (append (compile-ir e env)
                             (compile-ir p env)
                             (list `(set-car!)
                                   `(push_lit #!void)))))
                (begin
                  (set! fs (- fs 1))
                  ir-code)))

             (($set-cdr! ,p ,e)
              (let ((ir-code
                     (append (compile-ir e env)
                             (compile-ir p env)
                             (list `(set-cdr!)
                                   `(push_lit #!void)))))
                (begin
                  (set! fs (- fs 1))
                  ir-code)))

	     (($symbol->string ,e)
              (append (compile-ir e env)
                      (list `(push_tag 1)
                            `(sub))))
	     
             (($integer->char ,e)
              (append (compile-ir e env)
                      (list `(push_tag 2)
                            `(add))))
	     (($make-vector ,len ,val)
	      (append (compile-ir len env)
		      (compile-ir len env)
		      (begin
			(set! fs (- fs 1))
			(list `(alloc)
			      `(get_heap)
			      `(pop_mem);;on assigne la taille
			      `(get_heap);;on retourne la position du heap
			      `(push_tag 5)
			      `(add)
			      `(get_heap)
			      `(push_mem);;on recupere la taille
			      `(push_lit 1)
			      `(add);;on ajoute 1 a la taille
			      `(get_heap)
			      `(add);;on calcul la nouvelle position du heap
			      `(set_heap);;on positionne le heap
			      ))
		      (compile-ir val env)
		      (begin
			(set! fs (- fs 1))
			(list `(assign_vec)))
		      ))
	     (($vector-length ,v)
	      (append (compile-ir v env)
		      (list '(push_tag 5)
			    '(sub)
			    '(push_mem))))
	     
	     (($vector-ref ,v ,pos)
	      (begin (set! fs (+ 0 fs))
		     (append (compile-ir v env)
			     (compile-ir pos env)
			     (begin
			       (set! fs (- fs 1))
			       (list '(push_lit 1)
				     '(add)
				     '(push_tag 5)
				     '(sub)
				     '(add)
				     '(push_mem))))))
	     (($vector-set! ,v ,pos ,val)
	       (begin (set! fs (+ 0 fs))
		     (append ;;(list `(comment ,(string-append "string set " (string c))))
		      (compile-ir val env)
		      (compile-ir v env);;on récupère la position de str
		      (compile-ir pos env);;on calcul la position du caractère a modifier
		      (begin
			(set! fs (- fs 2))
			(list '(push_lit 1)
			      '(add)
			      '(push_tag 5)
			      '(sub)
			      '(add) ;;on détermine l'adresse (la représentation nous la garde
			      '(pop_mem)
                              `(push_lit #!void)))))) ;;on doit mettre dans le tas à la position
	     (($vector? ,v)
	       (append (compile-ir v env)
		       (list '(get_tag))
		       (list '(push_tag 5))
		       (list '(cmp))
		       (list '(equal?))))
	     
             (($char->integer ,e)
              (append (compile-ir e env)
                      (list `(push_tag 2)
                            `(sub))))
	     
             ((if ,cond ,E0)
              (compile-ir `(if ,cond ,E0 #!void) env))
             ;;(let ((labend (label-gensym)))
             ;;  (append (compile-ir cond env)
             ;;         (compile-ir '#f '())
             ;;          (list '(cmp))
             ;;  	 (list `(jmpe ,labend))
             ;;          (compile-ir E0 env)
             ;;          (list `(lab ,labend)))))
             
	     ((if ,cond ,E0 ,E1)
	      (let ((labfalse (label-gensym)) (labend (label-gensym)))
		(append (compile-ir cond env)
			(compile-ir '#f '())
			(begin
			  (set! fs (- fs 2))
			  (list '(cmp)))
			(list `(jmpe ,labfalse))
			(compile-ir E0 env)  ;; expression si vrai
			(list `(jmp ,labend))
			(list `(lab ,labfalse))
			(begin
			  (set! fs (- fs 1))
			  (list '(fs-adjust)))
			(compile-ir E1 env)  ;; expression si faux
			(list `(lab ,labend)))))
             
	     (($println ,ex)
              (begin
                (append (compile-ir ex env)
                        (list '(println)))))
             
             (($+ ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(add)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($- ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(sub)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($* ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(mul)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($quotient ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(quotient)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
	     (($remainder ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(remainder)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($modulo ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(modulo)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             
             (($= ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(cmp))
			     (list '(equal?)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($< ,p1 ,p2)
	      (let ((comp-ir 
		     (append (compile-ir p1 env)
			     (compile-ir p2 env)
			     (list '(cmp))
			     (list '(less?)))))
		(begin (set! fs (- fs 1))
		       comp-ir)))
             
             (($number? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 0))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($char? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 2))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($string? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 3))
                      (list '(cmp))
                      (list '(equal?))))
	     (($symbol? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 4))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($pair? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 6))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($procedure? ,e1)
              (append (compile-ir e1 env)
                      (list '(get_tag))
                      (list '(push_tag 7))
                      (list '(cmp))
                      (list '(equal?))))
             
             (($null? ,e1)
              (append (compile-ir e1 env)
                      (list '(null?)
                            '(cmp)
                            '(equal?))))

             (($eq? ,e1 ,e2)
              (let ((ir-code
                     (append (compile-ir e1 env)
                             (compile-ir e2 env)
                             (list '(cmp))
                             (list '(equal?)))))
                (begin
                  (set! fs (- fs 1))
                  ir-code)))
	     
             ;; pairs and non-empty lists
             ((quote ,lit) when (not (or (null? lit) (symbol? lit)))
              (let* ((const (gen-const lit))
                     (const-defs
                      (if (null? const-code)
                          '()
                          (compile-ir-program (reverse const-code) env)))
                     (const-ir (compile-ir const env)))
                (begin
                  ;;(set! fs (+ fs 1))
                  (append const-defs const-ir))))

             ;; empty list
             ((quote ,lit) when (null? lit)
              (let ((ir-code (list `(push_lit ,lit))))
                (begin
                  (set! fs (+ fs 1))
                  ;; (display fs)
                  ;; (display " --> ")
                  ;; (pp lit)
                  ir-code)))

             ;; symbol
	     ((quote ,lit) when (symbol? lit)
	      (begin
		(set! fs (+ fs 1))
		(list `(push_lit ,lit))))
	     
             (($make-string ,size)
	      (append (compile-ir size env) ;;on calcul la taille
		      (compile-ir size env) ;;pour allocation
		      (begin (set! fs (- fs 1))
			     (list '(comment "debut make-string ")
				   '(alloc);;allocation
				   `(get_heap)
				   `(pop_mem);;on assigne la taille
				   `(get_heap);;on retourne la position du heap
				   `(push_tag 3)
				   `(add)
				   `(get_heap)
				   `(push_mem);;on recupere la taille
				   `(push_lit 1)
				   `(add);;on ajoute 1 a la taille
				   `(get_heap)
				   `(add);;on calcul la nouvelle position du heap
				   `(set_heap);;on positionne le heap
				   '(comment "fin make-string")
				   )))) ;;on crée la chaîne
             
	     (($string-length ,s)
	      (begin (set! fs (+ 0 fs))
		     (append (compile-ir s env)
			     (list '(push_tag 3)
				   '(sub)
				   '(push_mem)))))
	     (($write-char ,c)
	      (append (compile-ir c env)
		      (begin
			(set! fs (+ fs 0))
			(list '(putchar)
			      '(push_lit #!void)))))
	     
	     (($read-char)
	      (begin (set! fs (+ fs 1))
		     (list '(getchar))))
             
	     (($string-ref ,s ,pos)
	      (begin (set! fs (+ 0 fs))
		     (append (compile-ir s env)
			     (compile-ir pos env)
			     (begin
			       (set! fs (- fs 1))
			       (list '(push_lit 1)
				     '(add)
				     '(push_tag 3)
				     '(sub)
				     '(add)
				     '(push_mem))))))
	     
             ;; other litterals
             (,lit when (constant? lit)
                   (let ((ir-code (list `(push_lit ,lit))))
		     (begin
		       (set! fs (+ fs 1))
		       ;; (display fs)
		       ;; (display " --> ")
		       ;; (pp lit)
		       ir-code)))

             ;; variables
             (,var when (variable? var)
                   (let* ((var-pos (assoc var env))
                          (ir-code
                           (if var-pos
                               (list `(push_loc ,(- fs (cdr var-pos))))
                               (list `(push_glo ,(env-lookup genv var))))))
                     (begin
                       (set! fs (+ fs 1))
                       ;; (display fs)
                       ;; (display " --> ")
                       ;; (pp var)
                       ir-code)))

             ;; procedure calls
             ((,E0 . ,Es)
              (let* ((nb-params (length Es))
		     (old-fs fs)
                     (ir-code
                      (append (list `(comment ,expr))
                              (list `(comment "the arguments"))
                              (begin ;; (pp "debut args")
                                (push-on-stack-ir Es env))
                              (begin ;; (pp "fin args")
                                (compile-ir E0 env))
                              (list `(comment "the call"))
			      (begin ;;(set! fs (+ fs 1))
                                (list `(call ,nb-params))))))
                ;; (pp "proc")
                ;; (pp fs)
                (set! fs ( + 1 old-fs));(- fs nb-params))
                ;; (display "call ")
                ;; (pp nb-params)
                ;; (pp fs)
                ir-code)))))

(define lambda-count 0)
(define label-count 0)
(define label-gensym
  (lambda ()
    (set! label-count (+ label-count 1))
    (list "label_" (- label-count 1))))

;; generation lambda symbol
(define lambda-gensym 
  (lambda ()
    (begin
      (set! lambda-count (+ lambda-count 1))
      (string-append "lam_" (number->string (- lambda-count 1))))))

(define push-on-stack-ir
  (lambda (exprs env)
    (if (null? exprs)
        '()
        (append (compile-ir (car exprs) env)
                (push-on-stack-ir (cdr exprs) env)))))

(define range
  (lambda (start end)
    (if (> start end)
        '()
        (cons start (range (+ start 1) end)))))
;;(trace compile-ir)
;;(trace compile-ir-program)
;;(trace gen-const)
;;(trace gen-const-helper)
;;(trace compile-ir-body)
