(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 2 5)

(if #f 'f
    (+ 2 
       ;(sum-integers 3 5)
       (if #f 'f
           (+ 3
              ;(sum-int 4 5)
              (if #f 'f
                  (+ 4
                     ;(sum-int 5 5)
                     5
                     0))))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (sum f a dx b)
  (if (> a b)
      0
      (+ (f a) (sum f (dx a) dx b))))

;; Exercise 1.29

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-rule f a b n) 
  (define (poop start n) 
    (if (= n 0)
        (f a)
        (+ (* 
            (f (+ a (/ (- b a) n)))
            (cond 
              ((= n start) 1)
              ((even? n) 2) 
              (else 4)))
           (poop start (- n 1))))) 
  (* (/ (/ (- b a) n) 3.0)
     (poop n n)))

(define (simpsons-rule f a b n)
  (define (h n)
    (/ (- b a) n))
  (define (loop n)
    (if (= n 0)
        (f a)
        (+ (* (if (even? n) 2 4)
              (h n))
           (loop (- n 1)))))
  (* (/ (h n) 3.0)
     (+ (h n) (loop (- n 1)))))

(integral cube 2 10 4)

(simpsons-rule cube 2 10 4)