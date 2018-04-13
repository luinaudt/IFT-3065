(println 123)
((lambda ()
   (define write 5)
   (println write)
   ))
(println write)
(println 15)
;123
;5
;procedure
;15
