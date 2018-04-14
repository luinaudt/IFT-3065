(define x 11)
((lambda (x)
   (begin
     ((lambda (x)
        (begin
          (println x)
          (set! x 44)
          (println x)))
      33)
     (println x)))
 22)
(println x)

((lambda (x)
   (begin
     ((lambda (y)
        (begin
          (println x)
          (set! x 44)
          (println x)))
      33)
     (println x)))
 22)
(println x)

((lambda (z)
   (begin
     ((lambda (y)
        (begin
          (println x)
          (set! x 44)
          (println x)))
      33)
     (println x)))
 22)
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
