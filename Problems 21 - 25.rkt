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

(define (proper-divisors n)
  (let ((m (sqrt n)))
    (define (iter i)
      (cond
        ((< m i) '())
        ((divides? n i)
         (if (or (= i m) (= i 1))
             (cons i (iter (+ i 1)))
             (cons i (cons (/ n i) (iter (+ i 1))))))
        (else (iter (+ i 1)))))
    (iter 1)))
        
(define (sum-divisors n)
  (apply
   +
   (proper-divisors n)))

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

#| Problem 22
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
|#

#| Comment out because reading file messes with Racket trace

(define names
  (list->vector
   (sort
    (map (lambda (x) (string-trim x "\""))
	(string-split (file->string "p022_names.txt") ",")) comment out because messes with Racket trace
   string<?)))
|#

(define (name->score name)
  (apply
   +
   (map (lambda (x) (- (char->integer x) 64)) (string->list name))))

(define (total-names-score names)
  (define (iter total count)
    (if (= count (vector-length names))
        total
        (iter (+ total (* (+ count 1) (name->score (vector-ref names count)))) (+ count 1))))
  (iter 0 0))

#| Problem 23
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
|#

;Check if a number is abundant
(define (abundant? n)
  (< n (sum-divisors n)))

;Vector of abundant numbers up to 28123
;(define abundants
;  (list->vector (filter abundant? (range 1 28123))))

;For abundant number with pos n in abundants vector, sum it with every other abundant in the vector and set the value of the pos with the sum in abundant-sums-vector to 1

(define abundant-sums-vector
  (make-vector 28125 0))

(define (abundant-sums n)
  (define (iter count)
    (if (or (= count (vector-length abundants))
            (>= (+ (vector-ref abundants n) (vector-ref abundants count)) 28123))
        abundant-sums-vector
        (begin
          (vector-set! abundant-sums-vector (+ (vector-ref abundants n) (vector-ref abundants count)) 1)
          (iter (+ count 1)))))
  (iter n))

;Run abundant-sums on every abundant number in the abundants vector so the abundant-sums-vector is marked 1 for all pos that are an abundant sum
(define (all-abundant-sums n)
  (define (iter count)
    (if (> count (vector-length abundants))
        abundant-sums-vector
        (begin
          (abundant-sums count)
          (iter (+ count 1)))))
  (iter n))

(define (not-abundant-sums range)
  (cond ((null? range)
         '())
        ((= (vector-ref abundant-sums-vector (car range)) 0)
         (cons (car range) (not-abundant-sums (cdr range))))
        (else
         (not-abundant-sums (cdr range)))))

;Run (apply + (not-abundant-sums (range 1 28123))) for answer - kinda hacky because it sets pos 28123 in abundant-sums-vector to 0 when should be 1, fix later

#| Problem 24
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
|#

(define digits
  (list->vector '(0 1 2 3 4 5 6 7 8 9)))

(define (compare permutation i)
  (if (= i 0)
      #f
      (> (vector-ref permutation i) (vector-ref permutation (- i 1)))))
 
(define (largest-index permutation)
  (define (iter i index)
    (cond ((= (vector-length permutation) i)
           index)
          ((and (compare permutation i) (> i index))
           (iter (+ i 1) i))
          (else
           (iter (+ i 1) index))))
  (iter 0 0))

(define (pivot permutation)
  (if (= 0 (largest-index permutation))
      0
      (- (largest-index permutation) 1)))

(define (pivot-successor permutation pivot)
  (define (iter j index)
    (cond ((= (vector-length permutation) j)
           index)
          ((and
            (< (vector-ref permutation pivot) (vector-ref permutation j))
            (< index j))
           (iter (+ j 1) j))
          (else
           (iter (+ j 1) index))))
  (iter (+ pivot 1) pivot))
  
(define (swap permutation i j)
  (let ((i-value (vector-ref permutation i))
        (j-value (vector-ref permutation j)))
    (vector-set! permutation i j-value)
    (vector-set! permutation j i-value)
    permutation))

(define vector-reverse!
  (lambda (v)
    (let ((len (vector-length v)))
      (do ((left-index 0 (+ left-index 1))
           (right-index (- len 1) (- right-index 1)))
          ((<= right-index left-index))
        (swap v left-index right-index))
      v)))

(define (pivot-swap-reverse permutation)
  (let* ((pivot (pivot permutation))
         (pivot-successor (pivot-successor permutation pivot))
         (swapped-permutation (swap permutation pivot pivot-successor)))
    (vector-append
     (vector-take swapped-permutation (+ pivot 1))
     (vector-reverse! (vector-drop swapped-permutation (+ pivot 1))))))

(define (nth-permutation permutation n)
  (if (= n 1)
      permutation
      (nth-permutation (pivot-swap-reverse permutation) (- n 1))))
    
#| Problem 25
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
|#

(define (fib-itr n)
  (define (iter count a b)
    (if (= count 0)
        b
        (iter (- count 1) (+ a b) a)))
  (iter n 1 0))

(define (n-digits? number digits)
  (= digits (length (n-to-digits number))))

(define (find-fib-n-digits digits)
  (define (iter start)
    (if (n-digits? (fib-itr start) digits)
        start
        (iter (+ start 1))))
  (iter 12))
  



