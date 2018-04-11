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
;; Makes sure stack is empty at the end.
(define (compile-ir-program exprs env)
  (if (null? exprs)
      '()
      (append (compile-ir (car exprs) env)
              (begin
                (set! fs 0)
                (list `(check-stack-integrity 0)))
              (compile-ir-program (cdr exprs) env))))


;; Called whenever a bloc of code is being compiled (lets and lambda).
;; Makes sure the result of each expression is being consumed and returns
;; with a single value on the stack.
(define (compile-ir-body exprs env)
  ;; (display fs)
  ;; (display "   ")
  ;; (if (not (null? exprs))
  ;;     (pp (car exprs)))
  (cond ((null? exprs)
         (error "empty body"))
        ((null? (cdr exprs))
         (compile-ir (car exprs) env))
        (else
         (append (compile-ir (car exprs) env)
                 (compile-ir-body (cdr exprs) env)))))


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
	      (let* ((var-val (assoc var genv))
                     (ir-code
                      (if var-val
                          (append (list `(comment ("re-def " ,var)))
                                  (compile-ir ex env)
                                  (list `(pop_glo ,(cdr var-val))))
                          (let ((name gcnt))
                            (append (begin
                                      (set! genv (cons (cons var gcnt) genv))
                                      (set! gcnt (+ gcnt 1))
                                      (compile-ir ex env))
                                    (list `(comment ("def " ,var)))
                                    (list `(pop_glo ,name)))))))
                (begin
                  (set! fs (- fs 1))
                  ir-code)))
	     
             ((lambda ,params . ,body)
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

             ((let ,bindings . ,body)
              (let* ((vars (map car bindings))
                     (vals (map cadr bindings))
                     (ref-ids (range (+ fs 1) (+ fs (length vars))))
                     (extended-env (append (map cons vars ref-ids) env))
                     (target-fs (+ fs 1)))
                ;; push vals on stack and compile body with extended environment
                (append (list `(comment "let"))
			;;(begin
			;;  (set! fs (+ fs 1))
			;;  (list '(save-cont 0)))
			(push-on-stack-ir vals env)
                        (compile-ir-body body extended-env)
			;;(list `(rest-cont ,target-fs))
                        (begin
                          (set! fs target-fs)
                          (list `(check-stack-integrity ,target-fs))
			  ))))

             ((make-closure ,code . ,fv)
              (let* ((fv-cnt (length fv))
                     (ir-code
                      (append (compile-ir code env)
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
                             (list '(cons)))))
                (begin
                  (set! fs (- fs 1))
                  ir-code)))

             (($car ,p)
              (append (compile-ir p env)
                      (list '(car))))

             (($cdr ,p)
              (append (compile-ir p env)
                      (list '(cdr))))

             (($set-car! ,p ,e)
              (let ((ir-code
                     (append (compile-ir e env)
                             (compile-ir p env)
                             (list `(set-car!)))))
                (begin
                  (set! fs (- fs 2))
                  ir-code)))

             (($set-cdr! ,p ,e)
              (let ((ir-code
                     (append (compile-ir e env)
                             (compile-ir p env)
                             (list `(set-cdr!)))))
                (begin
                  (set! fs (- fs 2))
                  ir-code)))
             
             ((if ,cond ,E0)
              (compile-ir `(if ,cond ,E0 #!void)))
             ;;(let ((labend (label-gensym)))
             ;;  (append (compile-ir cond env)
             ;;          (compile-ir '#f '())
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
			(compile-ir E0 env)
			(list `(jmp ,labend))
			(list `(lab ,labfalse)) ;;faux
			(compile-ir E1 env)
			(list `(lab ,labend))   ;;fin
			(begin
			  (set! fs (- fs 1))
			  (list '(fs-adjust))))))
             
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
             
             (($boolean? ,e1)
              (append (compile-ir e1 env)
                      (list '(boolean?)
                            '(cmp)
                            '(equal?))))
             
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
             ((quote ,lit) when (not (null? lit))
              (let* ((const (gen-const lit))
                     (const-defs
                      (if (null? const-code)
                          '()
                          (compile-ir-body (reverse const-code) env)))
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
             (($make-string ,size)
	      (append (compile-ir size env) ;;on calcul la taille
		      (list '(comment "debut make-string ")
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
			    ))) ;;on crée la chaîne
	     (($string-length ,s)
	      (begin (set! fs (+ 0 fs))
		     (append (compile-ir s env)
			     (list '(push_tag -3)
				   '(add)
				   '(push_mem)))))
	     
	     (($string-set! ,s ,pos ,c) 
	      (begin (set! fs (+ 0 fs))
		     (append (list `(comment ,(string-append "string set " (string c))))
			     (compile-ir c env)
			     (compile-ir s env);;on récupère la position de str
			     (compile-ir pos env);;on calcul la position du caractère a modifier
			     (begin
			       (set! fs (- fs 3))
			       (list '(push_lit 1)
				     '(add)
				     '(push_tag -3)
				     '(add)
				     '(add) ;;on détermine l'adresse (la représentation nous la garde
				     '(pop_mem)))))) ;;on doit mettre dans le tas à la position
	     
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
                              (list `(call ,nb-params)))))
                ;; (pp "proc")
                ;; (pp fs)
                (set! fs (- fs nb-params))
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
