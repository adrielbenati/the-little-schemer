(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else
        (cons (car l)
              (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l))
            (rember* a (cdr l)))))))

(define position
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) 1)
     (else
      (add1 (position a (cdr lat)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old (cons new (cdr l))))
       (else
        (cons (car l)
              (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l))
            (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (add1 (occur* a (cdr l))))
       (else
        (occur* a (cdr l)))))
     (else
      (plus (occur* a (car l))
            (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (subst* new old (cdr l))))
       (else
        (cons (car l)
              (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l))
            (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (insertL* new old (cdr l))))
       (else
        (cons (car l)
              (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
            (insertL* new old (cdr l)))))))
