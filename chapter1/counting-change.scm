
(define (count-change amount)
  (define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)(= kinds-of-coins 0)) 0)
        (else(+ (cc amount (- kinds-of-coins 1))
                (cc (- amount (first-denomination kinds-of-coins))
                    kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 20)
        ((= kinds-of-coins 5) 50)
        ))
  (cc amount 5))

(define (formulae n)
  (cond ((< n 3) n)
        (else (+ (formulae (- n 1))
                 (* (formulae (- n 2)) 2)
                 (* (formulae (- n 3)) 3)))))

(define (formulae-i n)
    (define (iter a b c count)
    (if (= count 0)
        c
        (iter b c (+ c
                     (* b 2)
                     (* a 3)) (- count 1))))
    (cond ((< n 3) n)
        (else (iter 0 1 2 (- n 2)))))


(define (pascal-triangle row col)
  (cond ((= row col) 1)
        ((= col 1) 1)
        (else (+ (pascal-triangle (- row 1) col)
                 (pascal-triangle (- row 1) (- col 1))
                 ))))

(define (sine angle)
  (define (cube x)
    (* x x x))
  (define (p x)
    (- (* 3 x)
       (* 4 (cube x))))
  (if (< angle 0.01)
      angle
      (p (sine (/ angle 3)))))

(define (fast-expt-i b n)
  (define (even? b)
    (= (remainder n 2) 0))
  (define (iter b r count)
    (if (= count 0)
        r
        (iter b (* r b) (- count 1))))
  (if (even? b)
      (iter (square b) 1 (/ n 2))
      (* b (fast-expt-i b (- n 1)))))

(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (fast-multi a n)
  (define (even? a)
    (= (remainder a 2) 0))
  (cond ((= n 0) 0)
        ((even? a) (fast-multi (double a) (halve n)))
        (else (+ a (fast-multi a (- n 1))))))

(define (fast-mul-i a n)
  (define (even? a)
    (= (remainder a 2) 0))
  (define (iter a r count)
    (if (= count 0)
        r
        (iter a (+ a r) (- count 1))))
  (cond ((= n 0) 0)
        ((even? a) (iter (double a) 0 (halve n)))
        (else (+ a (fast-mul-i a (- n 1))))))

(define (fib n)
  (define (fib-iter a b p q count)
    (define (even? a)
      (= (remainder a 2) 0))
    (cond ((= count 0) b)
          ((even? count) (fib-iter a
                                   b
                                   (+ (* q q) (* p p))
                                   (+ (* 2 p q) (* q q))
                                   (/ count 2)))
          (else (fib-iter (+ (* a (+ p q)) (* b q))
                          (+ (* a q) (* b p))
                          p
                          q
                          (- count 1)))))
    (fib-iter 1 0 0 1 n)
)

