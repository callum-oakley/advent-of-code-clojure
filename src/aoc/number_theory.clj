(ns aoc.number-theory
  (:require
   [clojure.test :refer [deftest is]]))

;; Expects pairs of [a n] and finds x such that x = a (mod n)
(defn chinese-remainder-theorem [system]
  (loop [x 0 step 1 system system]
    (if-let [[a n] (first system)]
      (if (= a (mod x n))
        (recur x (* n step) (rest system))
        (recur (+ x step) step system))
      x)))

(deftest test-chinese-remainder-theorem
  (is (= 39 (chinese-remainder-theorem [[0 3] [3 4] [4 5]]))))
