;;fichier contenant la table des primitives schemes

(define (primitive? x)
  prims)

(define prims '($println
		$+
		$-
		$*
		$quotient
		$modulo
		$=
		$<
		$number?
		$read-char
		$write-char
		$integer->char
		$char->integer
		$char?
		$make-string
		$string-ref
		$string-set!
		$string-length
		$string?
		$cons
		$car
		$cdr
		$set-car!
		$set-cdr!
		$pair?
		$procedure?
		$eq?))
