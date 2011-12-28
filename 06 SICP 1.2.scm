;; Exercise 1.14

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;(cc 11 5)
(+ ;(cc 11 4)
 (+ ;(cc 11 3)
  (+ ;(cc 11 2)
   (+ ;(cc 11 1)
    ;(cc 11 0)
    0
    ;(cc (- 11 1) 1)
    (+ ;(cc 10 0)
     0
     (cc (- 10 1) 1); --> etc.
     )
    )
   (cc (- 11 5) 2)
   )
  ;(cc (- 11 10) 3)
  (+ ;(cc 1 1)
   (+ ;(cc 1 0)
    0
    ;(cc (- 1 1) 1)
    0
    )
   ;(cc (- 1 2))
   0
   )
  )
 ;(cc (- 11 25) 4)
 0
 )
;(cc (- 11 50) 5)
; 0     
;)

;; Exercise 1.17
(define (multiply a b)
  (if (= b 0)
      0
      (+ a
         (multiply a
                   (- b 1)))))

;(multiply 2 2)
(+ 2
   ;(multiply 2 1)
   (+ 2
      ;(multiply 2 0)
      0)