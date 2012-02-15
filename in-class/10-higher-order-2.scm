(define (square x) (* x x))

;; Exercise 1.34

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

;(f f) ; -> boom
; (f 2)
;   (2 2)

;; Exercise 1.36

(define foo (let ((x 4))
              (lambda (y)
                (+ y x))))

(foo 2)

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(let ((x 2))
 (fixed-point (lambda (y) (average y (/ x y)))
             1.0))

(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)

(newline)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)