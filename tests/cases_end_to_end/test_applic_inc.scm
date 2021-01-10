(define hello (lambda () "hello"))
(hello)

(define swap_pair 
    (lambda (pair) 
        (cons (cdr pair) (car pair))))
(swap_pair '(1 2))

(define var1 '(5))
var1
(set-car! var1 3)
var1
(define var2 '(5))
var2