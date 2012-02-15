(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define tolerance .000001)

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

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; ((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;; ((deriv square) 4)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; Exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(newtons-method (cubic 1 2 3) 1)

(define (double f)
  (lambda (x)
    (f (f x))))

;; Exercise 1.41

(define (inc x) (+ 1 x))
(((double (double double)) inc) 5)

;; Exercise 1.42

(define (compose f g) (λ (x) (f (g x))))
((compose square inc) 6)

;; Exercise 1.43

(define (repeated f n)
  (if (= 1 n)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

;; Exercise 1.44

(define (avg3 x y z) (/ (+ x y z) 3))

(define (smooth f)
  (λ (x)
    (avg3 (f (- x dx))
          (f x)
          (f (+ x dx)))))

; wrong
(define (n-fold-smooth f n)
  (repeated (smooth f)
            n))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((n-fold-smooth square 3) 6)
((n-fold-smooth square 6) 3)
