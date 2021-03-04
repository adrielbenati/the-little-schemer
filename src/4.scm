(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (minus n (sub1 m)))))))

(define tup?
  (lambda (tup)
    (cond
     ((null? tup) #t)
     ((number? (car tup)) (tup? (cdr tup)))
     (else #f))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (plus (car tup)
            (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (plus n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1)
           (null? tup2))
      (quote ()))
     (else
      (cons (+ (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2)))))))
