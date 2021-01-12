(define foo (lambda (x)
				(cons
					(begin (lambda () (set! x 1) 'void))
					(lambda () x))))
(define p (foo 2))

(cons ((cdr p)) (cons  ((car p)) (cons ((cdr p)) '())))