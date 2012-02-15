;; Initial Definitions
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (square x)
  (* x x))

(define (cube x) (* x x x))

(define (avg)
  (λ x 
    (/ (apply + x) (length x))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;Pairs

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

;;Representing rational numbers

(define (make-rat% n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (negative? n) 
                (negative? d))
           (cons (abs (/ n g)) 
                 (abs (/ d g))))
          ((and (positive? n) 
                (positive? d))
           (cons (/ n g) 
                 (/ d g)))
          (else 
           (cons (- (abs (/ n g)))             
                 (abs (/ d g)))))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define one-eigth (make-rat (- 1) (- 8)))
(define neg-one-half (make-rat 1 (- 2)))

;; 2.12 Abstraction Barriers

;; Exercise 2.2

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))
(define point-a (make-point 1 1))
(define point-b (make-point 1 5))
(define point-c (make-point 5 5))
(define point-d (make-point 5 1))
(define s1 (make-segment point-a point-b))
(define s2 (make-segment point-b point-c))
(define s3 (make-segment point-c point-d))
(define s4 (make-segment point-d point-a))
(define sx (make-segment point-a point-c))

(define (midpoint-segment x)
  (make-point
   ((avg) (x-point (start-segment x)) (x-point (end-segment x)))
   ((avg) (y-point (start-segment x)) (y-point (end-segment x)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3

(define (make-rectangle h w) (cons h w))
(define (height-rectangle x) (car x))
(define (width-rectangle x) (cdr x))
(define (make-height seg1 seg2) (cons seg1 seg2))
(define (make-width seg1 seg2) (cons seg1 seg2))
(define r1 
  (make-rectangle
   (make-height s1 s3)
   (make-width s2 s4)))
(define (length seg)
  (cond ((= (car (car seg)) (car (cdr seg)))
         (if (> (cdr (car seg)) (cdr (cdr seg)))
             (- (cdr (car seg)) (cdr (cdr seg)))
             (- (cdr (cdr seg)) (cdr (car seg)))))
        ((= (cdr (car seg)) (cdr (cdr seg)))
         (if (> (car (car seg)) (car (cdr seg)))
             (- (car (car seg)) (car (cdr seg)))
             (- (car (cdr seg)) (car (car seg)))))
        (else "Error: Segment is not horizontal/vertical")))

(define (perimeter r)
  (+ (* 2 (length (car (car r))))
     (* 2 (length (car (cdr r))))))

(define (area r)
  (* (length (car (car r)))
     (length (car (cdr r)))))

(define (make-quad x-axis-start y-axis-start height width)
  (define y-axis-end (+ height y-axis-start))
  (define x-axis-end (+ width x-axis-start))
  (cons 
   (make-height 
    (make-segment
     (make-point x-axis-start y-axis-start)
     (make-point x-axis-start y-axis-end))
    (make-segment
     (make-point x-axis-end y-axis-end)
     (make-point x-axis-end y-axis-start)))
   (make-width
    (make-segment
     (make-point x-axis-start y-axis-end)
     (make-point x-axis-end y-axis-end))
    (make-segment
     (make-point x-axis-start y-axis-start)
     (make-point x-axis-end y-axis-start)))))

(define r2 (make-quad 1 3 4 13))

;; 2.1.3

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

;; Exercise 2.4

(define (cons x y)
  (λ (m) (m x y)))

(define (car z)
  (z (λ (p q) p)))

;(car (cons x y))
;((λ (m) (m x y)) (λ (p q) p))
;((λ (p q) p) x y)
;x

(define (cdr z)
  (z (λ (p q) q)))

;; Exercise 2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; Exercise 2.6

(define zero (λ (f) (λ (x) x)))

(define (add-1 n)
  (λ (f) (λ (x) (f ((n f) x)))))

;(add-1 zero)
;(λ (f) (λ (x) (f (((λ (f) (λ (x) x)) f) x))))
