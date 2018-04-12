(define chaine "la classe")
(println (string? "essai"))
(println (string? (string-length "essai")))
(println (string? chaine))
(println (string-length "essai"))
(println (string-length chaine))
(string-set! chaine 5 #\U)
(println chaine)
(let ((chaine "bonjour")) 
  (begin
    (println chaine)
    (string-set! chaine 4 #\K)
    (println chaine)))
(println (string-ref "bon" 2))
(println (string-ref chaine 5))
(println (string-ref chaine 0))
(define test (make-string 15))
(println (string-length (make-string 9)))
(println (string-length test))
(string-set! test 11 #\5)
(println (string-ref test 11))
;#t
;#f
;#t
;5
;9
;la clUsse
;bonjour
;bonjKur
;n
;U
;l
;9
;15
;5
