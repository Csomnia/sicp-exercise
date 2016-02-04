(load "test-manager/load.scm")

(define (search f neg-point pos-point)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.001))
  (let ((midpoint (/ (+ neg-point pos-point) 2)))
    (if (close-enough? pos-point neg-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((> test-value 0)(search f neg-point midpoint))
                ((< test-value 0)(search f midpoint pos-point))
                (else midpoint))))
  ))

(define (helf-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0))
           (search f a b))
          ((and (< b-value 0) (> a-value 0))
           (search f b a))
          (else
           (error "VAlues are not of opposite singn " a b)))
    ))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (iter guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
          next
          (iter next))))
  (iter first-guess))
()
(define (my-sqrt x)                      
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 
               1))
     

;; (fixed-point (lambda (x) (+ 1 (/ 1 x)))
;;               1))
;; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;; (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)

(define (cont-frac n d k)
  (define (iter count)
    (if (= count k)
        (/ (n count) (d count))
        (/ (n count) (+ (d count) (iter (+ 1 count))))))
  (iter 1)
  )
(define (gen-cont-frac n d k sign)
  (define (iter count)
    (if (= count k)
        (/ (n count) (d count))
        (/ (n count) (sign (d count) (iter (+ 1 count))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter result count)
    (if (= count 0)
        result
        (iter (/ (n count) (+ result (d count))) (- count 1))))
  (iter (/ (n k) (d k)) (- k 1))
  )
(define (di i)
  (let ((res (remainder i 3)))
    (if (= 2 res)
      (* 2.0 (+ 1 (/ (- i 2) 3)))
      1.0)
    ))
(define (cal-e k)
  (+ (gen-cont-frac (lambda (i) 1.0) di k +) 2))
(define (tanx x k)
  (* (/ 1 x) (gen-cont-frac (lambda (i) (square x)) (lambda (i) (- (* 2 i) 1)) k -)))

(define (average-damp f)
  (lambda (x) (/ (+ (f x) x) 2)))

(define (sqrt-abs x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (deriv f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f (- x dx))) (* 2 dx))))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g 0.000001) x)))))

(define (newton-method g)
  (fixed-point (newton-transform g) 1.0))

(define (newton-sqrt x)
  (newton-method (lambda (y) (- (* y y) x))))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (double p)
  (lambda (x) (p (p x)))
  )
(define (compose p2 p1)
  (lambda (x) (p2 (p1 x))))

(define (repeated f n)
  (if  (= n 1)
       f
       (compose f (repeated f (- n 1)))
       ))

(define dx 0.00001)
(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3))
  )
(define (inc x)
  (+ 1 x))

(define (3th-root x)
  (fixed-point-abs ((repeated average-damp 1) (lambda (y) (/ x (* y y))))
               1.5))

(define (iterative-improve f-good-enough f-improve-guess)
  (lambda (guess)
    (if (f-good-enough guess (f-improve-guess guess))
         guess
         ((iterative-improve f-good-enough f-improve-guess) (f-improve-guess guess))
     ))
  )




(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (fixed-point-abs f guess)
  ((iterative-improve close-enough? f) guess))

