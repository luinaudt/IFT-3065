(define x '(1 2 3))
(define inc (lambda (x) (+ x 1)))

(println (map not '(#f #t #f)))
(println (map (lambda (x) (+ x 1)) '(1 2 3)))
(println (map inc x))

;(#t #f #t)
;(2 3 4)
;(2 3 4)
