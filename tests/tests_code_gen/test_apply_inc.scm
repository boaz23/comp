(apply (lambda () 'ok) '())
(apply (lambda (x y z) (cons y (cons x z))) 1 "a" '(ok))

(define funny
  (lambda (n)
    (if (zero? n)
        '()
        (apply cons `(ha ,(funny (- n 1)))))))

(define funnier
  (lambda (n)
    (if (zero? n)
        '()
        `(ha ,@(apply cons `(ha ,(funny (- n 1))))))))

(funny 5)
(funnier 5)