

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