(define x 11)
(let ((x 22))
  (let ((x 33))
    (println x)
    (set! x 44)
    (println x))
  (println x))
(println x)

(let ((x 22))
  (let ((y 33))
    (println x)
    (set! x 44)
    (println x))
  (println x))
(println x)

(let ((z 22))
  (let ((y 33))
    (println x)
    (set! x 44)
    (println x))
  (println x))
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