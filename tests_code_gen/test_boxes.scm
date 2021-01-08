(define f (lambda (n)
      (lambda ()
        (cons
          n
          (cons (lambda ()
            (set! n (+ n 1))
            n)
          (cons (lambda ()
            (set! n 0)) '()))))))
(define x (f 5))
x
(set! x (x))
x
(car x)
((car (cdr x)))
(car x)
((car (cdr (cdr x))))
(car x)

(define g (lambda (n)
    (cons
      n
      (cons (lambda ()
        (set! n (+ n 1))
        n)
      (cons (lambda ()
        (set! n 0)) '())))))
(g 5)