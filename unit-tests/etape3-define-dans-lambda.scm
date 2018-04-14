(println 123)

((lambda ()
   (define write 5)
   (let ((x 10) (write 20))
     (define write 40)
     (println write))
   (println write)))

(write "write devrait être une procédure ici")
(write-char #\newline)

(println 15)

;123
;40
;5
;"write devrait être une procédure ici"
;15
