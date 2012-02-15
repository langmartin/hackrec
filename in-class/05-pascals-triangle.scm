(load "../sentences.scm")

(define (second sent)
  (first (butfirst sent)))

(define (f row)
  (if (empty? (butfirst row))
      1
      (sentence
       (+ (first row)
          (second row))
       (f (bf row)))))

(define (pascal n)
  (cond ((= n 1) '(1))
        ((= n 2) '(1 1))
        (else
         (sentence
          1
          (f (pascal (- n 1)))))))

(define (pascal-triangle row col)
  (cond ((> col row) 0)
        ((< col 0) 0)
        ((= col 1) 1)
        ((+ (pascal-triangle (- row 1) (- col 1))
            (pascal-triangle (- row 1) col)))))
