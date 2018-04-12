(println 
 (letrec
     ((loop
       (lambda (x)
	 (+ x 5))))
   (loop 8)))

;13
