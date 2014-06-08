(define (rel-apply a rel)
  (cond
    ((null? rel) '())
    ((equal? a (car (car rel))) (cadr (car rel)))
    (else (rel-apply a (cdr rel)))))

(define (cons-membership a b)
  (let ((A (if (procedure? a) a (lambda (x) (equal? x a))))
        (B (if (procedure? b) b (lambda (x) (equal? x b)))))
    (lambda (x) (or (A x) (B x)))))

(define (make-e-fun transit e-transit)
  (lambda (state . char)
    (if (null? char)
      (rel-apply state e-transit)
      (rel-apply (list state (car char)) transit))))

(define (membership set)
  (lambda (a)
    (member a set)))

(define (make-nfa Q S T q0 F) (list Q S T q0 F))
(define (state M)      (car M))
(define (alphabet M)   (car (cdr M)))
(define (transition M) (car (cdr (cdr M))))
(define (start M)      (car (cdr (cdr (cdr M)))))
(define (accept M)     (car (cdr (cdr (cdr (cdr M))))))

(define (transit M r . a)
  (if (null? a)
    ((transition M) r)
    ((transition M) r (car a))))

(define (accept-state? M a) ((accept M) a))

(define (make-configration M state str) (list M state str))
(define (conf-machine conf) (car conf))
(define (conf-state conf)   (car (cdr conf)))
(define (conf-string conf) (car (cdr (cdr conf))))

(define (acceptable-conf? conf)
  (accept-state? (conf-machine conf) (conf-state conf)))

(define (empty-string-conf? conf)
  (null? (conf-string conf)))

(define (make-initial-configration M str)
  (make-configration M (start M) str))

(define (next-configs conf)
  (let ((M   (conf-machine conf))
        (r   (conf-state conf))
        (str (conf-string conf)))
    (append
      (map (lambda (state)
             (make-configration M state (cdr str)))
           (transit M r (car str)))
      (map (lambda (state)
             (make-configration M state str))
           (transit M r)))))

(define (conf-accepts? conf)
  (if (empty-string-conf? conf)
    (acceptable-conf? conf)
    (letrec ((acc? (lambda (configs)
                     (cond
                       ((null? configs) #f)
                       ((conf-accepts? (car configs)) #t)
                       (else (acc? (cdr configs)))))))
      (acc? (next-configs conf)))))

(define (accepts? M str)
  (let ((lst (if (string? str)
               (string->list str)
               str)))
    (conf-accepts? (make-initial-configration M lst))))

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
              (alphabet M)
              delta
              q0
              (cons-membership q0 (accept M)))))

;(define (sum M1 M2)
;  (let ((delta (lambda (q . a)
;                 (if (null? a)
;                   (cons ((transition M1) (car q))
;                         ((transition M2) (cdr q)))
;                   (cons ((transition M1) (car q) a)
;                         ((transition M2) (car q) a))))))
;    (make-nfa (cons (start M1) (start

(define l '(((0 #\0) (1))
            ((1 #\0) (2))
            ((2 #\1) (3))
            ))
(define M (make-nfa (membership '(0 1 2 3))
                    (membership '(#\0 #\1))
                    (make-e-fun l '())
                    0
                    (membership '(3))))
(define N (star M))
