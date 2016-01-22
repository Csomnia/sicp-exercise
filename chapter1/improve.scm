;;; improve.scm

(define (average x y)
  (/ (+ x y)
     2))

(define (improve guess x)
  (average guess (/ guess x)))
