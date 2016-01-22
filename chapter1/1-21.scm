(load "test-manager/load.scm")

(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (square (fast-expt b (/ (- n 1) 2)))))
        ))

(define (slow-expt b n)
  (cond ((= n 0) 1)
        (else (* (slow-expt b (- n 1)) b))
        ))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (define (divides? test-divisor)
    (= (remainder n test-divisor) 0))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor) test-divisor)
          (else (find-divisor (next test-divisor)))
          ))
  (find-divisor 2))


(define (expmod base exp m)
  (define (not-normal-square-root? a n)
  (cond ((and (not (= a 1))
             (not (= a (- n 1)))
             (= (remainder (square a) n) 1)) true)
        (else false)
     ))
  (cond ((= exp 0) 1)
        ((not-normal-square-root? base m) 0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (expmod-slow base exp m)
  (remainder (fast-expt base exp) m))
(define (ex-squ x)
  (* x x))
(define (expmod-s1ow base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (ex-squ (expmod-s1ow base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod-s1ow base (- exp 1) m))
                    m)
              )))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1))))
  )

(define (mr-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (mr-fast-prime n times)
  (cond ((= 0 times) true)
        ((mr-test n) (mr-fast-prime n (- times)))
        (else false)))

(define (fast-prime n times)
  (cond ((= 0 times) true)
        ((fermat-test n) (fast-prime n (- times 1)))
        (else false)))


(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))
(define (start-prime-test n start-time)
  (if (fast-prime n 100)
      (report-prime (- (real-time-clock) start-time))))
(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))


(define (scolpe-check n upper)
  (define (odd? number)
    (= (remainder number 2) 1))
  (define (iter n upper)
    (if (odd? n)
        (time-prime-test n))
    (scolpe-check (+ n 1) upper)
    )
  (if (> n upper)
      (display "done")
      (iter n upper)
      ))


;;; 1000000007***93      normal dectect       
;;; 1000000007***60      call next function
;;; 1000000007***1257

;;; 10000000019***275
;;; 10000000019***186
;;; 10000000019***1479

;;; 100000000003***872
;;; 100000000003***577
;;; 100000000003***1619

;;; 1000000000039***2796
;;; 1000000000039***1844
;;; 1000000000039***1714

;;; 10000000000037***8719
;;; 10000000000037***5762
;;; 10000000000037***1854






