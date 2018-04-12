(println
 (let loop ((x 0))
   (and (or 1 2)
        (or x 4))))

(println
 (let loop ()
   (and (or 1 2)
        (or 0 4))))

;0
;0
   
