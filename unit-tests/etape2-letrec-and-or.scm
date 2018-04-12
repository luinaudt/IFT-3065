(println
 (letrec ((loop
           (lambda (x)
             (and (or 1 2)
                  (or x 4)))))
   (loop 3)))

(println
 (letrec ((loop
           (lambda ()
             (and (or 1 2)
                  (or 3 4)))))
   (loop)))

;3
;3
