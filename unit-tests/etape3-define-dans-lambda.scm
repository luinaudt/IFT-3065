(println 123)
(define k "write devrait etre la procedure")
(define l 698)
((lambda ()
   (define write 5)
   (let ((x 10) (write 20))
     (define write 40)
     (println write))
   (println write)))
(write 128)
(println "")
(write l)
(println "")
(write k)
(println "")
(write "write devrait être une procédure ici")
(write-char #\newline)

(println 15)

;123
;40
;5
;128
;698
;"write devrait être une procédure ici"
;15
