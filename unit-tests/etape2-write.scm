(write-char #\ )
(write-char #\space)
(write-char #\5)
(write-char #\newline)
($write-char #\ )
($write-char #\space)
($write-char #\5)
($write-char #\newline)
(write #\5)
(write-char #\newline)
(write "bonjour")
(write-char #\newline)
(write -5)
(write-char #\newline)
(write 125)
(write-char #\newline)
(write 9)
(write-char #\newline)
(write -145)
(write #f)
(write #t)
(write-char #\newline)
(write '(4 5 "456" 9))
(write-char #\newline)
(write '(4 #\5 5 "456" (98 89 9 "hjds")))
(write-char #\newline)
(write '( ))
(write-char #\newline)
(write 'bonbon)
(write-char #\newline)
(write println)
(write-char #\newline)
(write "bon\"jour\n hello")
(write-char #\newline)
(write #\space)
(write-char #\newline)
(write '(1 2 . 3))
(write-char #\newline)
(write #\newline)
(write-char #\newline)
(define str "test---")
(string-set! str 5 #\space)
(write str)
(write-char #\newline)
;  5
;  5
;#\5
;"bonjour"
;-5
;125
;9
;-145#f#t
;(4 5 "456" 9)
;(4 #\5 5 "456" (98 89 9 "hjds"))
;()
;bonbon
;#<procedure >
;"bon\"jour\n hello"
;#\space
;(1 2 . 3)
;#\newline
;"test- -"
