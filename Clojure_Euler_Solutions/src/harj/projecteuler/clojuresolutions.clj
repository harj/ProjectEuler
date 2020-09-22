(ns harj.projecteuler.clojuresolutions)

(comment
  Problem 1
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.)

(defn multiple-3-5 [n]
  (or (= (mod n 3) 0)
      (= (mod n 5) 0)))

(defn sum-multiples [limit]
  (->>
    (range 1 1000)
    (filter multiple-3-5)
    (reduce +)))