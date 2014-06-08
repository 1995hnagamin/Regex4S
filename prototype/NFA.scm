(define (make-fun arrow)
  (letrec ((A (lambda (r a ls)
                (cond
                  ((null? ls) '())
                  ((equal? (list r a) (caar ls)) (cadar ls))
                  (else (A r a (cdr ls)))))))
    (lambda (r a)
      (A r a arrow))))

(define l '(((0 #\0) (1)) ((0 #\1) (0))
            ((1 #\0) (2)) ((1 #\1) (0))
            ((2 #\0) (2)) ((2 #\1) (3))
            ((3 #\0) (3)) ((3 #\1) (3))))

(define (make-nfa Q S T q0 F) (list Q S T q0 F))
(define (state M)      (car M))
(define (alphabet M)   (car (cdr M)))
(define (transition M) (car (cdr (cdr M))))
(define (initial M)    (car (cdr (cdr (cdr M)))))
(define (stacpt M)     (car (cdr (cdr (cdr (cdr M))))))

(define (transit M r a) ((transition M) r a))
(define (accept-state? M a) (member a (stacpt M)))

(define (make-configration M state str) (list M state str))
(define (conf-machine conf) (car conf))
(define (conf-state conf)   (car (cdr conf)))
(define (conf-string conf) (car (cdr (cdr conf))))

(define (acceptable-conf? conf) (accept-state? (conf-machine conf) (conf-state conf)))
(define (empty-string-conf? conf) (null? (conf-string conf)))

(define (make-initial-configration M str)
  (make-configration M (initial M) str))

(define (next-configs conf)
  (let ((M   (conf-machine conf))
        (r   (conf-state conf))
        (str (conf-string conf)))
    (map (lambda (state)
           (make-configration M state (cdr str)))
         (transit M r (car str)))))

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

(define M (make-nfa '(0 1 2 3) '(#\0 #\1) (make-fun l) 0 '(3)))
