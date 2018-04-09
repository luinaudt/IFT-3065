;;; File: compiler.scm

;;;============================================================================

;; The run function executes a process and captures its exit
;; status and output.  This is used to execute the gcc compiler.
;;(include "macros.scm")

(include "macros-extension.scm")

(define (run path . args)
  (let* ((port
          (open-process (list path: path
                              arguments: args)))
         (output
          (read-line port #f))
         (status
          (process-status port)))
    (close-port port)
    (cons status output)))

;;;----------------------------------------------------------------------------

;; Source code parser.

(define (parse-program filename)
  (call-with-input-file
      filename
    (lambda (port)
      (parse-exprs port))))

(define (parse-exprs port)
  (let ((x (read port)))
    (if (eof-object? x)
	'() ;; fin du programme
	(cons x
	      (parse-exprs port)))))

;;;----------------------------------------------------------------------------

(define gcc-link-option
  (if (equal? (cadr (system-type)) 'apple)
      "-Wl,-no_pie"
      "-Wall"))

(define (compile filename)

  (let* ((ast           (append (parse-program "lib.scm")(list '(comment "debut")) (parse-program filename)))
         (closed-ast    (map closure-conv (map assign-conv (map alpha-conv ast))))
         (expanded-ast  (expand-macros closed-ast))
         (ir-code       (begin (display "\n") (pp expanded-ast) (display "\n") (compile-ir-program expanded-ast '())))
         (code          (begin (display "\n")
			       (pp ir-code)
			       (display "\n") (compile-program ir-code lambda-env))))

  ;; (let* ((ast           (append (parse-program "lib.scm") (parse-program filename)))
  ;;        (closed-ast    (map closure-conv (map assign-conv (map alpha-conv ast))))
    ;;        (expanded-ast  (expand-maccoderos closed-ast))
  ;;        (ir-code       (compile-ir-program expanded-ast '()))
  ;;        (code          (compile-program ir-code lambda-env)))
    
    (let* ((base-filename  (path-strip-extension filename))
           (asm-filename   (string-append base-filename ".s"))
           (exe-filename   (string-append base-filename ".exe"))
           (root-dir       (path-directory (this-source-file))))

      ;; write generated assembler code

      (with-output-to-file asm-filename (lambda () (print code)))

      ;; assemble generated assembler code and link with stdio.o

      (let ((x (run "gcc"
                    "-m64"
                    gcc-link-option
                    "-o"
                    exe-filename
                    (path-expand "stdio.o" root-dir)
		    (path-expand "mmap.o" root-dir)
                    asm-filename)))

        ;; return exit status

        (car x)))))

(define (main filename)
  (compile filename))

;;;============================================================================

;(trace compile)
