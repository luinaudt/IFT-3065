(define nb 1)
(println nb)
(set! nb (+ nb 1))
(println nb)
(set! nb (* nb 65))
(println nb)
(let ((x 5))
  (begin
    (println x)
    (set! x 6)
    (println x)))
(define + println)
(+ 5)
(set! - println)
(- 15)
;1
;2
;130
;5
;6
;5
;15
