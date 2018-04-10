(let ((x 12)) (println x) (println 15))
(let ((x 56) (y 45)) (println (+ x y)) (println x) (println y))
(let ((t println) (x 55) (y 45)) (t 10) (t x) (t (+ x y)))
;12
;15
;101
;56
;45
;10
;55
;100
