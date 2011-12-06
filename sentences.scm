;;;; Assert
(define (disp text)
  (for-each (lambda (t)
              (display t)
              (display #\space))
            text))

(define (wtf . text)
  (disp text)
  (car "wtf: fatal error"))

(define-syntax assert
  (syntax-rules (=>)
    ;; the value of exp must equal expected
    ((_ (exp => expected))
     (let ((result exp))
       (if (not (equal? result expected))
           (wtf "assert" 'exp "got" result "not" expected))))
    ;; an exp all by itself must not be #f
    ((_ exp)
     (if (not exp)
         (wtf "assert" 'exp "is false")))
    ;; recurse through multiple expressions
    ((_ e1 e2 ...)
     (begin
       (assert e1)
       (assert e2 ...)))))

;;;; Sentences
(define (sentence . args)
  (define (lp sent)
    (cond ((null? sent)
           '())
          ((null? (car sent))
           (lp (cdr sent)))
          ((pair? (car sent))
           (append (lp (car sent))
                   (lp (cdr sent))))
          (else
           (cons (car sent)
                 (lp (cdr sent))))))
  (lp args))

(assert
 ((sentence 'foo)                     => '(foo))
 ((sentence '(foo bar (baz zup)))     => '(foo bar baz zup))
 ((sentence '(foo bar (baz zup) zow)) => '(foo bar baz zup zow))
 ((sentence '(((foo) () ())))         => '(foo)))

(define sentence? list?)
(define word? symbol?)

;; This makes words into sentences of individual letters so that
;; first, last, etc operate on them correctly. It leaves sentences
;; alone.
(define (%sentence obj)
  (if (sentence? obj)
      obj
      (map (lambda (ch)
             (string->symbol
              (make-string 1 ch)))
           (string->list (symbol->string obj)))))

(define (first sent)
  (car (%sentence sent)))

(assert
 ((first '(foo bar)) => 'foo)
 ((first 'foo)       => 'f))

(define (butfirst sent)
  (cdr (%sentence sent)))

(assert
 ((butfirst '(foo bar)) => '(bar))
 ((butfirst 'foo)       => '(o o)))

(define (last sent)
  (define (lp sent)
    (if (null? (cdr sent))
        (car sent)
        (lp (cdr sent))))
  (lp (%sentence sent)))

(assert
 ((last '(foo bar)) => 'bar)
 ((last 'foo)       => 'o))

(define (butlast sent)
  (define (lp sent)
    (if (null? (cdr sent))
        '()
        (cons (car sent)
              (lp (cdr sent)))))
  (lp (%sentence sent)))

(assert
 ((butlast '(foo bar))         => '(foo))
 ((butlast '(foo bar baz zup)) => '(foo bar baz))
 ((butlast 'foo)               => '(f o)))

(define empty? null?)
(define bf butfirst)
(define bl butlast)
