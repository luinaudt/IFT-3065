;; (println
;;  (let loop ((x 0))
;;    (or 1 2)))
;; 
;; ;2

expanded ast 
((define println (lambda (x) ($println x)))
 (define cons (lambda (x y) ($cons x y)))
 (println ((let ((loop 0))
             (let ((g1 (lambda (x)
                         (let ((g2 1))
                           (if g2 g2 2)))))
               (set! loop g1)
               loop))
           0)))

closed-ast 
((define println (make-closure (lambda ($this g3) ($println g3))))
 (define cons (make-closure (lambda ($this g4 g5) ($cons g4 g5))))
 (let (($clo println))
   ((closure-code $clo)
    $clo
    (let (($clo (let ((g9 0))
                  (let ((g6 (let (($clo cons))
                              ((closure-code $clo) $clo g9 '()))))
                    (let ((g1 (make-closure
                               (lambda ($this g7)
                                 (let ((g8 1))
                                   (if g8 g8 2)))
                               (let (($clo g8))
                                 ((closure-code $clo) $clo)))))
                      (let (($clo set-car!))
                        ((closure-code $clo) $clo g6 g1))
                      (let (($clo car))
                        ((closure-code $clo) $clo g6)))))))
      ((closure-code $clo) $clo 0)))))
