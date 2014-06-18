(load "./rx4sx.scm")

(define l '(((0 #\0) (1))
            ((1 #\0) (2))
            ((2 #\1) (3))
            ))

(define (ll state . char)
  (cond
    ((null? char) '())
    ((not (number? (car char))) '())
    ((and (even? (car char)) (equal? 0 state)) '(1))
    ((and (odd?  (car char)) (equal? 1 state)) '(2))
    (else '())))

(define M (make-nfa (make-membership '(0 1 2))
                    ll
                    0
                    (make-membership '(2))))
(define N (star M))

(define (n . str) (accepts? N str))
