(use util.queue)

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

(define (qconf-accepts? M qconf)
  (print (queue->list qconf))
  (cond
    ((queue-empty? qconf) #f)
    ((and (empty-string-conf? (queue-front qconf))
          (acceptable-conf? M (queue-front qconf))) #t)
    (else
      (let ((next (next-configs M (queue-pop! qconf))))
        (if (null? next)
          (qconf-accepts? M qconf)
          (qconf-accepts? M (apply queue-push! (cons qconf next))))))))

(define (accepts? M str)
  (let ((lst (if (string? str)
               (string->list str)
               str)))
    (conf-accepts? M (make-initial-configration M lst))))

(define (qaccepts? M lst)
  (qconf-accepts? M (queue-push! (make-queue)
                                 (make-initial-configration M lst))))

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

