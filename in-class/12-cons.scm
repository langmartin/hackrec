(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (negative? n) (negative? d))
           (cons (abs (/ n g)) (abs (/ d g))))
          ((and (positive? n) (positive? d))
           (cons (/ n g) (/ n g)))
          (else
           (cons (- (abs (/ n g))) (abs (/ n g)))))))

(define (make-rat n d)
  (let ((g ((if (< d 0) - +)
            (gcd n d))))
    (cons (/ n g) (/ d g))))

;; 2.2

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;; Exercise 2.2

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

;(define p1 (make-point 2 3))
;(define p2 (make-point 5 6))
;(define s1 (make-segment p1 p2))

; Jacob's fancy avg
(define (avg . x)
  (/ (apply + x) (length x)))

(define (avg x y)
  (/ (+ x y) 2))

(define (midpoint-segment seg)
  (make-point (avg (x-point (start-segment seg))
                   (x-point (end-segment seg)))
              (avg (y-point (start-segment seg))
                   (y-point (end-segment seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3

(define (len seg)
  (cond ((= (x-point (start-segment seg))
            (cadr seg))
         (if (> (cdr (car seg)) (cddr seg))
             (- (cdr (car seg)) (cddr seg))
             (- (cddr seg) (cadr seg))))
        ))

(define (len-dimen dimen seg)
  (let ((x1 (dimen (start-segment seg)))
        (x2 (dimen (end-segment seg))))
    (abs (- x1 x2))))

(define (len seg)
  (let ((x (len-dimen x-point seg))
        (y (len-dimen y-point seg)))
    (if (= x 0) y x)))

(define (make-rectangle t r b l)
  (cons (cons t r)
        (cons b l)))

(define (rec-top rec) (car (car rec)))
(define (rec-right rec) (car (cdr rec)))
(define (rec-bottom rec) (cdr (car rec)))
(define (rec-left rec) (cdr (cdr rec)))

(define (perimeter rec)
  (+ (len (rec-top rec))
     (len (rec-right rec))
     (len (rec-bottom rec))
     (len (rec-left rec))))

(define (area rec)
  (* (len (rec-top rec)
          (rec-right rec))))

;; Exercise 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car cell)
  (cell (lambda (x y) x)))

(define (cdr cell)
  (cell (lambda (x y) y)))

(car (car (cons (cons 1 2) 3)))

;; Exercise 2.5
