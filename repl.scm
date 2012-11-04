(define env (environment))

(define repl
  (lambda a
    (begin
      (write "little> ")
      (let ((r (read)))
           (let ((e (eval r env)))
                (print "=>" e)))
      (repl))))

(repl)
