(define hello (lambda () "hello"))
(hello)

(define swap_pair
    (lambda (pair)
        (cons (cdr pair) (car pair))))
(swap_pair '(1 2))