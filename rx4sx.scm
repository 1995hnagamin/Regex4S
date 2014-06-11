(define (rel-apply a rel)
  (cond
    ((null? rel) '())
    ((equal? a (car (car rel))) (cadr (car rel)))
    (else (rel-apply a (cdr rel)))))

(define (make-membership set) (lambda (e) (member e set)))
(define (cons-membership a membership)
  (lambda (e) (or (equal? e a)
                  (membership e))))

(define (make-e-fun transit e-transit)
  (lambda (state . char)
    (if (null? char)
      (rel-apply state e-transit)
      (rel-apply (list state (car char)) transit))))

(define (make-nfa Q T q0 F) (list Q T q0 F))
(define (state M)      (car M))
(define (transition M) (car (cdr M)))
(define (start M)      (car (cdr (cdr M))))
(define (accept M)     (car (cdr (cdr (cdr M)))))

(define (transit M r . a)
  (if (null? a)
    ((transition M) r)
    ((transition M) r (car a))))

(define (accept-state? M a) ((accept M) a))

(define (make-configration state str) (list state str))
(define (conf-state conf)  (car conf))
(define (conf-string conf) (car (cdr conf)))

(define (acceptable-conf? M conf)
  (accept-state? M (conf-state conf)))

(define (empty-string-conf? conf)
  (null? (conf-string conf)))

(define (make-initial-configration M str)
  (make-configration (start M) str))

(define (make-configs list-of-configs) list-of-configs)
(define (cons-configs config configs) (cons config configs))
(define (car-configs configs) (car configs))
(define (cdr-configs configs) (cdr configs))
(define empty-configs? null?)

(define (next-configs M conf)
  (display (conf-state conf))
  (display " ")
  (print (conf-string conf))
  (let ((r   (conf-state conf))
        (str (conf-string conf)))
    (append
      (map (lambda (state)
             (make-configration state str))
           (transit M r))
      (map (lambda (state)
             (make-configration state (cdr str)))
           (transit M r (car str))))))

(define (conf-accepts? M conf)
  (if (empty-string-conf? conf)
    (acceptable-conf? M conf)
    (letrec ((acc? (lambda (configs)
                     (cond
                       ((empty-configs? configs) #f)
                       ((conf-accepts? M (car-configs configs)) #t)
                       (else (acc? (cdr-configs configs)))))))
      (acc? (next-configs M conf)))))

(define (accepts? M str)
  (let ((lst (if (string? str)
               (string->list str)
               str)))
    (conf-accepts? M (make-initial-configration M lst))))

(define (new-state state)
  (letrec ((A (lambda (n)
                (if (state n)
                  (A (+ 1 n))
                  n))))
    (A 0)))

(define (star M)
  (let* ((q0 (new-state (state M)))
         (transit (transition M))
         (delta
           (lambda (q . a)
             (cond
               ((equal? q q0) (if (null? a) (list (start M)) '()))
               ((accept-state? M q) (if (null? a)
                                      (cons (start M) (transit q))
                                      (transit q (car a))))
               (else (if (null? a) (transit q) (transit q (car a))))))))
    (make-nfa (cons-membership q0 (state M))
              delta
              q0
              (cons-membership q0 (accept M)))))

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
