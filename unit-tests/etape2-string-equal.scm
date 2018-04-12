(println (string=? "allo" "allo"))
(println (string=? "" ""))
(println (string=? "allo" "aaaa"))
(println (string=? "aaaa" "allo"))
(println (string=? "allo" "allo2"))
(println (string=? "allo2" "allo"))
;#t
;#t
;#f
;#f
;#f
;#f
