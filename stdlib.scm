(define quote
  (macro (a) e a))

(define list
  (lambda l l))

(define let
  (macro (var val body) e
         (eval (list (list 'lambda (list var) body) val) e)))

(define not
  (lambda (b) (if b #f #t)))

(define or
  (macro (a b) e
         (let a (eval a e)
              (if a a (eval b e)))))

(define and
  (macro (a b) e
         (let a (eval a e)
              (if (not a) #f (eval b e)))))

(define map
  (lambda (f l)
    (if (null? l) '()
        (cons (f (car l)) (map f (cdr l))))))

(define else #t)

(define cond2if
  (lambda (l)
    (if (null? l) #<undefined>
        (list 'if (car (car l)) (car (cdr (car l))) (cond2if (cdr l))))))

(define cond
  (macro conds e
         (eval (cond2if conds) e)))
