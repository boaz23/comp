(define f (lambda (a b . c) (cons (+ a b) (cons c '()))))

(f 1 2)