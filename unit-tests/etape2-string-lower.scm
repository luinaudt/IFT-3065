(println (string<? "allo" "allo"))
(println (string<? "" ""))
(println (string<? "allo" "aaaa"))
(println (string<? "aaaa" "allo"))
(println (string<? "allo" "allo2"))
(println (string<? "allo2" "allo"))
;#f
;#f
;#f
;#t
;#t
;#f
