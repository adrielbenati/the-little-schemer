(define atom?
  (lambda (x)
	(and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
	(cond
	 ((null? l) #t)  ; If (null? l) is #t, then lat? is #t
	 ((atom? (car l)) (lat? (cdr l))) ; Recursion!
	 (else #f))))

(define member?
  (lambda (a lat)
	(cond
	 ((null? lat) #f) ;If (null? lat) is #t, then member? is #f
	 (else (or (eq? (car lat) a) ; Recursion using (or)
			   (member? a (cdr lat)))))))
