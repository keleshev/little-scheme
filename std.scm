(define cons
  (vau car_cdr e (make #<pair>
                       (eval (car car_cdr) e)
                       (eval (car (cdr car_cdr)) e))))

(define atom?
  (vau a e (eq? (type (eval (car a) e)) '())))

(define pair?
  (vau a e (eq? (type (eval (car a) e)) #<pair>)))

(define primitive?
  (vau a e (eq? (type (eval (car a) e)) #<primitive>)))

(define procedure?
  (vau a e (eq? (type (eval (car a) e)) #<procedure>)))

(define quote
  (vau a e (car a)))

(define environment
  (vau a e e))

(define lambda
  (vau para_body e
       (vau args e2
              (eval (car (cdr para_body))
                    (cons (if (atom? (car para_body))
                              (cons (cons (car para_body) '())
                                    (cons (evop args e2) '()))
                              (cons (car para_body) (evop args e2)))
                          e2)))))

(define evop
  (vau l_env e
       (if (null? (eval (car l_env) e)) '()
           (cons (eval (car (eval (car l_env) e))
                       (eval (car (cdr l_env)) e))
                 (evop (cdr (eval (car l_env) e))
                       (eval (car (cdr l_env)) e))))))

(define apply
  (vau f_a e
       (eval (cons (car f_a) (evop (eval (car (cdr f_a)) e) e)) e)))

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
  (vau l e
       (eval (cons (list 'lambda
                         (map first (car l))
                         (car (cdr l)))
                   (map second (car l)))
             e)))

(define not
  (lambda (b) (if b #f #t)))

(define or
  (vau a_b e
       (let ((a (eval (car a_b) e)))
            (if a a (eval (cadr a_b) e)))))

(define and
  (vau a_b e
       (let ((a (eval (car a_b) e)))
            (if (not a) #f (eval (cadr a_b) e)))))

(define else #t)

(define cond2if
  (lambda (l)
    (if (null? l) #<undefined>
        (list 'if (car (car l)) (car (cdr (car l))) (cond2if (cdr l))))))

(define cond
  (vau conds e
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
(test (map add1 '(1 2 3)) => '(2 3 4))

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

(define write-pair
  (lambda (p)
    (begin (write (car p))
           (cond ((null? (cdr p)) 'ok)
                 ((pair? (cdr p)) (begin (put " ")
                                         (write-pair (cdr p))))
                 (else (begin (put " . ") (write (cdr p)))))
           #<void>)))

(define write
  (lambda (o)
    (begin
      (cond ((null? o) (put "()"))
            ((and (atom? o) (eq? o #<void>)) #<void>)
            ((atom? o) (put o))
            ((pair? o) (begin (put "(") (write-pair o) (put ")")))
            (else (put (type o))))
      #<void>)))

(define print
  (lambda args
    (begin (map (lambda (a) (begin (write a) (write " "))) args)
           (write "\n")
           #<void>)))
