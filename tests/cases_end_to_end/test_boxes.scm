(define f (lambda (n)
      (lambda ()
        (cons
          n
          (cons (lambda ()
            (set! n (+ n 1))
            n)
          (cons (lambda ()
            (set! n 0)) '()))))))
(define y (f 5))
y
(set! y (y))
y
(car y)
((car (cdr y)))
(car y)
((car (cdr (cdr y))))
(car y)

(define g (lambda (n)
    (cons
      n
      (cons (lambda ()
        (set! n (+ n 1))
        n)
      (cons (lambda ()
        (set! n 0)) '())))))
(g 5)

(define h (lambda (n)
      (lambda ()
        (cons
          (lambda () n)
          (cons (lambda ()
            (set! n (+ n 1))
            n)
          (cons (lambda ()
            (set! n 0)) '()))))))
(define x (h 5))
x
(set! x (x))
x
((car x))
((car (cdr x)))
((car x))
((car (cdr (cdr x))))
((car x))
