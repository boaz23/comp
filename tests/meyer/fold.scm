(define f
  (lambda (x y)
    `(f ,x ,y)))

(fold-left f 'unit '(x1 x2 x3 x4 x5 x6))
(fold-right f 'unit '(x1 x2 x3 x4 x5 x6))