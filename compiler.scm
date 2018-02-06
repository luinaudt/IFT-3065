;;; File: compiler.scm

;;;============================================================================

;; The run function executes a process and captures its exit
;; status and output.  This is used to execute the gcc compiler.

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

;;;----------------------------------------------------------------------------

(define gcc-link-option
  (if (equal? (cadr (system-type)) 'apple)
      "-Wl,-no_pie"
      "-Wall"))

(define (compile filename)

  (let* ((ast (parse-program filename)) ;; parse program
         (code (compile-program ast)))  ;; generate code

    (let* ((base-filename (path-strip-extension filename))
           (asm-filename (string-append base-filename ".s"))
           (exe-filename (string-append base-filename ".exe"))
           (root-dir (path-directory (this-source-file))))

      ;; write generated assembler code

      (with-output-to-file asm-filename (lambda () (print code)))

      ;; assemble generated assembler code and link with stdio.o

      (let ((x (run "gcc"
                    "-m64"
                    gcc-link-option
                    "-o"
                    exe-filename
                    (path-expand "stdio.o" root-dir)
                    asm-filename)))

        ;; return exit status

        (car x)))))

(define (main filename)
  (compile filename))

;;;============================================================================
