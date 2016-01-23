(load "test-manager/load.scm")

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (integral f a b dx)
  (define (next-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2)) next-dx b)))

(define (simpon-integral f a b n)
  (define h (/ (- b a) n))
  (define (inc n) (+ n 1))
  (define (simpon-term k)
    (cond ((= k 0) (f a))
          ((even? k) (* 2 (f (+ a (* k h)))))
          (else (* 4 (f (+ a (* k h)))))
          ))
  (* (/ h 3) (sum simpon-term 0 inc n)))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (product-iter (next a)
                      (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (inc t) (+ t 1))
  (define (ident t) t)
  (product ident 1 inc n))

(define (pi-product a b)
  (define (inc t) (+ t 1))
  (define (pi-term n)
    (square (/ (* 2 n) (- (* 2 n) 1))))
  (* (product pi-term a inc b) (/ 1 b) 4))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b)))
  )
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sp-product term a next b)
  (accumulate * 1 term a next b))
(define (sp-sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter (term a))
          (combiner (term a)
                    (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null-value
                    (filtered-accumulate combiner null-value term (next a) next b filter)))))

(define (prime? number)
  (define (next call-for)
    (if (= call-for 2)
        3
        (+ call-for 2)))
  (define (find-smallest-divisor divisor)
    (cond ((> (square divisor) number) number)
          ((= (remainder number divisor) 0) divisor)
          (else (find-smallest-divisor (next divisor)))))
  
  (= (find-smallest-divisor 2) number))


(define (prime-sum a b)
  (define (inc x) (+ x 1))
  (define (indent x) x)
  (filtered-accumulate + 0 indent a inc b prime?))

;;; gcd was a procedure that interpreter has built.

(define (coprime-sum n)
  (define (inc x) (+ x 1))
  (define (indent x) x)
  (define (test x)
    (newline)
    (display x)
    true)
  (define (coprime-judge x)
    (if (= (gcd x n) 1)
        (test x)
        false))
  (filtered-accumulate + 0 indent 1 inc n coprime-judge))
