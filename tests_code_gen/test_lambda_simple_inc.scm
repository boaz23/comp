(define f
  (lambda (a b)
    (lambda (var1) b)))
f
(define x (f 1 2))
x
(x "h")