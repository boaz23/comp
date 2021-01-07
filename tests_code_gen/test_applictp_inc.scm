(define z '())
(define f (lambda (x) (set! z x) (g)))
(define g (lambda () (h (cons z '()))))
(define h (lambda (x) (cons 1 x)))

(define p (lambda (x) (cons "hello" x)))

(p 'a)
(f 0.1)