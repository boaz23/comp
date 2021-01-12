(define with (lambda (s f) (apply f s)))

(define crazy-ack
  (letrec ((ack3
	    (lambda (a b c)
	      (cond
	       ((and (zero? a) (zero? b)) (+ c 1))
	       ((and (zero? a) (zero? c)) (ack-x 0 (- b 1) 1))
	       ((zero? a) (ack-z 0 (- b 1) (ack-y 0 b (- c 1))))
	       ((and (zero? b) (zero? c)) (ack-x (- a 1) 1 0))
	       ((zero? b) (ack-z (- a 1) 1 (ack-y a 0 (- c 1))))
	       ((zero? c) (ack-x (- a 1) b (ack-y a (- b 1) 1)))
	       (else (ack-z (- a 1) b (ack-y a (- b 1) (ack-x a b (- c 1))))))))
	   (ack-x
	    (lambda (a . bcs)
	      (with bcs
		(lambda (b c)
		  (ack3 a b c)))))
	   (ack-y
	    (lambda (a b . cs)
	      (with cs
		(lambda (c)
		  (ack3 a b c)))))
	   (ack-z
	    (lambda abcs
	      (with abcs
		(lambda (a b c)
		  (ack3 a b c))))))
    (lambda ()
      (and (= 7 (ack3 0 2 2))
	   (= 61 (ack3 0 3 3))
	   (= 316 (ack3 1 1 5))
	   (= 636 (ack3 2 0 1))
	   ))))

(crazy-ack)