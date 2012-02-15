(define (square n)
  (* n n))

(define (halve n)
  (/ n 2))

(define (fer n exp)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square
          (fer n (halve exp))))
        (else
         (* n
            (fer (- n 1) (- exp 1))))))

;(fer 2 2)
;(fer 2 3)
;(fer 2 4)

; (define (fmr n times)
  
(define (* a b)
  (if (= b 0)
      0
      (+ a
         (* a (- b 1)))))
        
; (* 2 2)
;(+ 2 ;(* 2 1)
;   (+ 2 ;(* 2 0)
;      0))

(define (fei n exp)
  (define (iter n exp final)
    (cond ((= exp 0) final)
          ((even? exp)
           (iter (square n) (halve exp) final))
        (else
         (iter n (- exp 1) (* n final)))))
  (iter n exp 1))

(fei 2 2)
(fei 2 3)
(fei 2 4)
(fei 2 6)

(define (double n) (+ n n))

(define (fmr n times)
  (cond ((= times 0) 0)
        ((even? times)
         ; (double
         ;  (fmr n (halve times)))
         (fmr (double n) (halve times))
         )
        (else
         (+ n
            (fmr n (- times 1))))))

(fmr 2 2)
(fmr 2 3)

(define (fmi n times)
  (define (iter n times final)
    (cond ((= times 0) final)
          ((even? times)
           (iter (double n) (halve times) final))
          (else
           (iter n (- times 1) (+ n final)))))
  (iter n times 0))

(fmi 2 2)
(fmi 2 3)
(fmi 2 100)
            

