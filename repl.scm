(define env (environment))

(define repl
  (lambda a
    (begin
      (put "little> ")
      (print "=>" (eval (read) env))
      (repl))))

(repl)
