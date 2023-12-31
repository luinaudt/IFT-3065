;;fichier contenant la table des primitives schemes

(define (primitive? x)
  (assoc x prims))

;;test si x est dans une liste y
(define (in-list x y)
  (if (null? y)
      #f
      (if (eq? x (car y))
	  #t
	  (in-list x (cdr y)))))

(define prims '(($println)
		($+)
		($-)
		($*)
		($quotient)
		($modulo)
		($=)
		($<)
		($number?)
		($read-char)
		($write-char)
		($integer->char)
		($char->integer)
		($char?)
		($make-string)
		($string-ref)
		($string-set!)
		($string-length)
		($string?)
		($cons)
		($car)
		($cdr)
		($set-car!)
		($set-cdr!)
		($pair?)
		($procedure?)
		($eq?)))
