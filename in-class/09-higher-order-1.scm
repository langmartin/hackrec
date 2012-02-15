;; 1.30

(define (sum% op seed term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (op (term a) result))))
  (iter a seed))

(define (sum term a next b)
  (sum% + 0 term a next b))

(define (product term a next b)
  (sum% * 1 term a next b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(pi-sum 1 1000)

;; 1.31

(define (factorial n)
  (define (inc n) (+ n 1))
  (product identity 1 inc n))

(factorial 3)

(define (inc x) (+ x 1))

(define (pi4 precision)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (* 4 (product term 1.0 inc precision)))

(pi4 1000)

;; 1.32

(define accumulate sum%)

;; 1.33

(define (fa combiner null-value filter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value)) 