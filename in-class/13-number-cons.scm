;; Exercise 2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define z (cons 2 3))

(define (gcp n part)
  (define (iter power)
    (if (not (= 0 (remainder n (expt part power))))
        (- power 1)
        (iter (+ power 1))))
  (iter 1))

(define (car cell)
  (gcp cell 2))

(define (cdr cell)
  (gcp cell 3))

(car (cons 16 7))

(cdr (cons 16 7))

;; Exercise 2.6