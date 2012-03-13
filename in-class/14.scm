(list 1 2 3)
(define foo
  (cons 1
        (cons 2
              (cons 3
                    (cons 4 '())))))
(cons 1 2)
(list (cons 'a 1) (cons 'b 2))

;; Ex 2.17

(define (last-pair lst)
  (if (null? lst)
      '()
      (let lp ((lst lst))
        (if (null? (cdr lst))
            lst
            (lp (cdr lst))))))

(last-pair (list 23 72 149 34))

;; Ex 2.18

(define (butlast items)
  (if (null? (cdr items))
      '()
      (cons (car items)
            (butlast (cdr items)))))

(define (reverse items)
  (if (null? items)
      '()
      (cons (car (last-pair items))
            (butlast items))))

(define (reverse items)
  (define (lp items rev)
    (if (null? items)
        rev
        (lp (cdr items)
            (cons (car items) rev))))
  (lp items '()))

(reverse (list 1 4 9 16 25))

;; Ex 2.19

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination car)
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; Ex 2.20

(define (same-parity x . items)
  (define (f parity? lst)
    (if (null? lst)
        '()
        (if (parity? (car lst))
            (cons (car lst)
                  (f parity? (cdr lst)))
            (f parity? (cdr lst)))))
  (cons x
        (f (if (odd? x) odd? even?)
           items)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; Ex 2.21

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))           

(define (square-list items)
  (map square items))

;; Ex 2.22

(define nil '())
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list '(1 2 3))

;; Ex 2.23

(define (4each proc list)
  (if (null? list)
      'undefined-sucka
      (begin
        (proc (car list))
        (4each proc (cdr list)))))

(define (d c) (display c) c)
(for-each d '(1 2 3 #f 5))
(4each d '(1 2 3 #f 5))

(define foo (list 1 (list 2 (list 3 4))))
(cdr foo)
(cadr foo)

;; Ex 2.24

'(1 3 (5 7) 9)

(cadr (caddr '(1 3 (5 7) 9)))

'((7))

(cons (cons 7 '())
      '())

(caar '((7)))

;; (define (cdadadadadar lst)
;;   (cdadar (cdadar (cdar lst))))

;; (cdadadadadar '(1 (2 (3 (4 (5 (6 7)))))))