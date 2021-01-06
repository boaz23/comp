(define f (lambda (x) (g x)))
(define g (lambda (x) (h (cons x '()))))
(define h (lambda (x) (cons 1 x)))

(define p (lambda (x) (cons "hello" x)))

(p 'a)
(f 0.1)