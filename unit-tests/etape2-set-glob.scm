(let ((x 5))
  (begin
    (println x)
    (set! x 6)
    (println x)))
(define + println)
(+ 5)
(set! - println)
(- 15)
;5
;6
;5
;15
