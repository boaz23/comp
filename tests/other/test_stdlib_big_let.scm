(define not
  (lambda (x) (if x #f #t)))

(define map
  (let ((null? null?)
	(car car) (cdr cdr)
	(cons cons) (apply apply))
  (letrec ((map-many
	    (lambda (f lists)
	      (if (null? (car lists))
		  '()
		  (cons
		   (apply f (map-one car lists))
		   (map-many f (map-one cdr lists))))))
	   (map-one
	    (lambda (f s)
	      (if (null? s)
		  '()
		  (cons (f (car s))
			(map-one f (cdr s)))))))
    (lambda (f . args)
      (map-many f args)))))


(define fold-left
  (let
    ((null? null?)
     (car car)
     (cdr cdr)
     (cons cons)
     (map map)
     (apply apply))
    (letrec
      ((fold-left
         (lambda (f acc lists)
           (if (null? (car lists))
            acc
            (fold-left
              f
              (apply f acc (map car lists))
              (map cdr lists))))))
      (lambda (f acc list1 . lists_rest)
        (fold-left f acc (cons list1 lists_rest))))))

(let ((flonum? flonum?) (rational? rational?)
      (exact->inexact exact->inexact)
      (fold-left fold-left) (map map)
      (_+ +) (_* *) (_/ /) (_= =) (_< <)
      (car car) (cdr cdr) (null? null?))
  (let ((^numeric-op-dispatcher
	 (lambda (op)
	   (lambda (x y)
	     (cond
	      ((and (flonum? x) (rational? y)) (op x (exact->inexact y)))
	      ((and (rational? x) (flonum? y)) (op (exact->inexact x) y))
	      (else (op x y)))))))
    (let ((normalize
	   (lambda (x)
	     (if (flonum? x)
		 x
		 (let ((n (gcd (numerator x) (denominator x))))
		   (_/ (_/ (numerator x) n) (_/ (denominator x) n)))))))
      (set! + (lambda x (normalize (fold-left (^numeric-op-dispatcher _+) 0 x))))
      (set! * (lambda x (normalize (fold-left (^numeric-op-dispatcher _*) 1 x))))
      (set! / (let ((/ (^numeric-op-dispatcher _/)))
		(lambda (x . y)
		  (if (null? y)
		      (/ 1 x)
		      (normalize (fold-left / x y)))))))
    (let ((^comparator
	  (lambda (op)
	    (lambda (x . ys)
	      (fold-left (lambda (a b) (and a b)) #t
			 (map (lambda (y) (op x y)) ys))))))
      (set! = (^comparator (^numeric-op-dispatcher _=)))
      (set! < (^comparator (^numeric-op-dispatcher _<))))))
