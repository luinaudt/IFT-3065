(define x 11)
((lambda (x)
   ((lambda (x)
    (println x)
    (set! x 44)
    (println x)) 33)
  (println x)) 22)
(println x)

((lambda (x)
   ((lambda (y)
    (println x)
    (set! x 44)
    (println x)) 33)
  (println x)) 22)
(println x)

((lambda (z)
   ((lambda (y)
    (println x)
    (set! x 44)
    (println x)) 33)
  (println x)) 22)
(println x)

;33
;44
;22
;11
;22
;44
;44
;11
;11
;44
;44
;44
