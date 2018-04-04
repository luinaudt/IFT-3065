(define z 5)
(define f (lambda (x) (+ x z)))
(let ((x 11))
  (let ((x (+ x x)))
    (let ((y (let ((x 2200))
               (+ x x))))
      (set! z (+ x z)))))

;4422
