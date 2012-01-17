;; Exercise 1.20

(define (remainder2 a b)
  (display ".")
  (remainder a b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
(gcd 40 ;(remainder 206 40) 
     6)
(gcd 6 ;(remainder 6 40)
     4)
(gcd 4 2)
(gcd 2 4)
2

(gcd 40 (remainder 206 40))

(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40))))

;; Exercise 1.21
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;; Exercise 1.25
(define (halve x) (/ x 2))

(define (fei n exp)
  (define (iter n exp final)
    (display ".")
    (cond ((= exp 0) final)
          ((even? exp)
           (iter (square n) (halve exp) final))
        (else
         (iter n (- exp 1) (* n final)))))
  (iter n exp 1))

(define fast-expt fei)

(define (square x)
  (* x x))

(define (expmod base exp m)
  (display ".")
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmodhack base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test2 n)
  (define (try-it a)
    (= (expmodhack a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (test n)
  (define x (+ 1 (random (- n 1))))
  (= (time (expmod x n n))
     (time (expmodhack x n n))))

;; Exercise 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))