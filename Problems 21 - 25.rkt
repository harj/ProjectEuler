#lang racket/load
(require reprovide/require-transformer/glob-in)
(require (glob-in "*.rkt"))
;(require trace)

#| Problem 21
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
|#

(define (divisors n)
  (define (iter count)
    (if (> count (floor (sqrt n)))
        '(1)
        (cond ((divides? n count)
               (cons (list count (/ n count)) (iter (+ count 1))))
              (else (iter (+ count 1))))))
  (iter 2))

(define (sum-divisors n)
  (apply
   +
   (flatten (divisors n))))

(define (amicable-number? n)
  (let* ((d-a (sum-divisors n))
         (d-b (sum-divisors d-a)))
  (if (and (= n d-b) (not (= n d-a)))
      n
      '())))

(define (amicable-pairs n)
  (define (iter count)
    (if (= count n)
        '()
        (cons (amicable-number? count) (iter (+ count 1)))))
  (iter 1))

(define (sum-amicable-pairs n)
  (apply
   +
   (flatten (amicable-pairs n))))