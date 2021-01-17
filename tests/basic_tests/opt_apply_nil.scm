(define sum
  (lambda (n s)
    (if (= n 0)
      s
      (sum (- n 1) (+ s n)))))

(define f (lambda () (sum 5 0)))
(define g (lambda l (f)))
(define h (lambda l (apply + l)))
(f)
(apply f '())
(g)
(apply g '())
(h)
(apply h '())
