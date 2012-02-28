(define zero (lambda (f) (lambda (x) x)))

(define (add1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x) x))
           f)
          x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x) (f x)))
           f)
          x)))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus n n2)
  (lambda (f)
    (lambda (x)
      ((n f)
       ((n2 f) x)))))

(define (mult n n2)
  (lambda (f)
    (lambda (x)
      ((n (lambda (f)
            (lambda (x)
              (mult n (minus n2 one)))))
       x))))

(define (square x) (* x x))

(define (dot x)
  (display "."))

((two dot) '())
(newline)
(((plus one two) dot) '())
(newline)

((one square) 2)
((two square) 2)
(((plus two one) square) 2)
