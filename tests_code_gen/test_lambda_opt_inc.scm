(define g (lambda (a b . c) c))
(g 1 2 'hello #\c)

(define f (lambda (a b . c) (cons (+ a b) (cons c '()))))
(f 1 2)
(f 1 2 'hello #\c)
