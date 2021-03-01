(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons old (cons new (cdr lat))))
     (else
      (cons (car lat)
            (insertR new old (cdr lat)))))))
