(load "sentences.scm")

(define (reverse word)
  (if (empty? word)
      '()
      (sentence (last word)
                (reverse (butlast word)))))

(sentence 'o
          (sentence 'o
                    (sentence 'f
                              '())))

(define (%rev word answer)
  (if (empty? word)
      answer
      (%rev (butlast word)
            (sentence answer
                      (last word)))))

(define (reverse word)
  (%rev word '()))

(reverse 'foo)

(%rev 'foo '())
(%rev 'fo  '(() o))
(%rev 'f   '(() o o))
(%rev '()  '(() o o f))
