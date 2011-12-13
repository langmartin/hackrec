(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (+a a b)
  (if (= a 0)
      b
      (inc (+a (dec a) b))))

; (+a 2 3) -> (inc (+a 1 3))
; (+a 1 3) -> (inc (+a 0 3))
; (+a 0 3) -> 3
(inc
 (inc
  3))

(inc ;(+a 1 3)
 (inc ;(+a 0 3)
      3))

(define (+b a b)
  (if (= a 0)
      b
      (+b (dec a) (inc b))))

;(+b 2 3)
;(+b 1 4)
(+b 0 5)
5

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;(fib 3)
(+ ;(fib 2)
    (+ ;(fib 1)
     1
       ;(fib 0)
     0)
   ;(fib 1)
    1)

(+ (+ 1 0)
   1)

(define (fibi n)
  (fi 1 0 n))

(define (fi a b count)
  (if (= count 0)
      b
      (fi (+ a b)
          a
          (- count 1))))
;(fi 1 0 3)
;(fi 1 1 2)
;(fi 2 1 1)
;(fi 3 2 0)
2

