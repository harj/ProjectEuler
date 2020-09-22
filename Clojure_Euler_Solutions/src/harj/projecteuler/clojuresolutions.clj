(ns harj.projecteuler.clojuresolutions)

(comment
  Problem 1
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.)

(defn multiple-3-5 [n]
  (or (= (mod n 3) 0)
      (= (mod n 5) 0)))

(defn sum-multiples [limit]
  "Sums multiples of 3 or 5 under the limit"
  (->>
    (range 1 1000)
    (filter multiple-3-5)
    (reduce +)))

(comment
  Problem 2
  Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be
  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

  By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.)

(defn fib [n]
  (loop [x [0 1]]
    (let [n-1 (last x)
          n-2 (nth x (- (count x) 2))]
      (if (<= (count x) n)
        (recur (conj x (+ n-1 n-2)))
        (+ n-1 n-2)))))

(defn fib-range [limit]
  (loop [x 1
         fibs []]
    (if (> (fib x) limit)
        fibs
        (recur (+ x 1) (conj fibs (fib x))))))

(defn sum-even-fibs [limit]
  (reduce + (filter even? (fib-range limit))))




