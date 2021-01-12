;;; folding-left-and-right.scm
;;; Matching the variadic procedures in Scheme

;;; Programmer: Mayer Goldberg, 2019

;;; > (test-folds fleft fright)
;;; ((fold-left:
;;;    (f (f (f (f (f unit x1 y1 z1) x2 y2 z2) x3 y3 z3) x4 y4 z4)
;;;       x5
;;;       y5
;;;       z5))
;;;   (fold-right:
;;;     (f x1
;;;        y1
;;;        z1
;;;        (f x2 y2 z2 (f x3 y3 z3 (f x4 y4 z4 (f x5 y5 z5 unit))))))
;;;   (match the builtin procedures: #t))

(define fleft
  (letrec ((run
	    (lambda (f unit ss)
	      (if (ormap null? ss)
		  unit
		  (run f
		       (apply f unit (map car ss))
		       (map cdr ss))))))
    (lambda (f unit . ss)
      (if (and (andmap list? ss)
	       (apply = (map length ss)))
	  (run f unit ss)
	  (error 'fold-left "The lists aren't of the same length!")))))

(define fright
  (letrec ((run
	    (lambda (f unit ss)
	      (if (ormap null? ss)
		  unit
		  (apply f
			 `(,@(map car ss)
			   ,(run f unit (map cdr ss))))))))
    (lambda (f unit . ss)
      (if (and (andmap list? ss)
	       (apply = (map length ss)))
	  (run f unit ss)
	  (error 'fold-right "The lists aren't of the same length!")))))

(define test-folds
  (let ((fold-left fold-left)
	(fold-right fold-right)
	(arguments
	 `(,(lambda s `(f ,@s))
	   unit
	   (x1 x2 x3 x4 x5)
	   (y1 y2 y3 y4 y5)
	   (z1 z2 z3 z4 z5))))
    (lambda (fleft fright)
      `((fold-left: ,(apply fleft arguments))
	(fold-right: ,(apply fright arguments))
	(match the builtin procedures:
		 ,(and (equal? (apply fold-left arguments)
			       (apply fleft arguments))
		       (equal? (apply fold-right arguments)
			       (apply fright arguments))))))))
