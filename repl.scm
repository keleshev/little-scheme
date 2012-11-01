(define env __env__)

(define repl
  (lambda a
    (begin
      (print 'little> space)
      (let ((r (read)))
           (let ((e (eval r env)))
                (begin (print '=> space e))))
      (print newline)
      (repl))))

(repl)
