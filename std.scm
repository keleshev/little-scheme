(define quote
  (macro (a) e a))

(define environment
  (macro a e e))

(define lambda
  (macro (para body) e
         (macro args e2
                (eval body
                      (cons (if (atom? para)
                                (cons (cons para '()) (cons (evop args e2) '()))
                                (cons para (evop args e2)))
                            e2)))))

(define evop
  (macro (l env) e
         (if (null? (eval l e)) '()
             (cons (eval (car (eval l e)) (eval env e))
                   (evop (cdr (eval l e)) (eval env e))))))

(define apply
  (macro (f a) e
         (eval (cons f (evop (eval a e) e)) e)))

(define first car)
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define second cadr)
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define third caddr)
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define fourth cadddr)
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

(define last
  (lambda (l)
    (if (null? (cdr l)) (car l)
        (last (cdr l)))))

(define begin
  (lambda l (last l)))

(define list
  (lambda l l))

(define map
  (lambda (f l)
    (if (null? l) '()
        (cons (f (car l)) (map f (cdr l))))))

(define let
  (macro l e
         (eval (cons (list 'lambda
                           (map first (car l))
                           (car (cdr l)))
                     (map second (car l)))
               e)))

(define not
  (lambda (b) (if b #f #t)))

(define or
  (macro (a b) e
         (let ((a (eval a e)))
              (if a a (eval b e)))))

(define and
  (macro (a b) e
         (let ((a (eval a e)))
              (if (not a) #f (eval b e)))))

(define else #t)

(define cond2if
  (lambda (l)
    (if (null? l) #<undefined>
        (list 'if (car (car l)) (car (cdr (car l))) (cond2if (cdr l))))))

(define cond
  (macro conds e
         (eval (cond2if conds) e)))

(define equal?
  (lambda (a b)
    (cond ((and (null? a) (null? b)) #t)
          ((or (null? a) (null? b)) #f)
          ((and (atom? a) (atom? b)) (eq? a b))
          ((or (atom? a) (atom? b)) #f)
          (else (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))))

(define => '=>)

(define test
  (lambda (left arrow right)
          (if (equal? left right) #<void>
              (write (list 'failed left arrow right)))))

(test (cond (#f 'no) (#f 'no) (else 'yes)) => 'yes)
(test (let ((a 1) (b 2) (c 3)) (list a b c)) => '(1 2 3))

(define filter
  (lambda (f? l)
    (cond ((null? l) '())
          ((f? (car l)) (cons (car l) (filter f? (cdr l))))
          (else (filter f? (cdr l))))))

(test (filter (lambda (a) (eq? a 7)) '(5 7 8 7)) => '(7 7))

(define any?
  (lambda (f? l)
    (cond ((null? l) #f)
          ((f? (car l)) #t)
          (else (any? f? (cdr l))))))

(define print (lambda args (begin (map write args) #<void>)))
