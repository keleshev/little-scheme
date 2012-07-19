(define pi 3)

(define let
  (macro (e var val body)
         (eval
           (cons (cons 'lambda (cons (cons var '()) (cons body '())))
                 (cons val '()))
           e)))

(define not
  (lambda (b) (if b #f #t)))

(define or
  (macro (e a b)
         (let a (eval a e)
              (if a a (eval b e)))))

(define and
  (macro (e a b)
         (let a (eval a e)
              (if (not a) #f (eval b e)))))

(define map
  (lambda (f l)
    (if (null? l) '()
        (cons (f (car l)) (map f (cdr l))))))
