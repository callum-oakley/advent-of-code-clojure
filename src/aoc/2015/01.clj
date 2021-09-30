(ns aoc.2015.01
  (:require
   [clojure.test :refer [deftest are is]]))

(defn part-1* [instructions]
  (->> instructions
       (map #(case % \( 1 \) -1))
       (reduce + 0)))

(defn part-2* [instructions]
  (->> instructions
       (map #(case % \( 1 \) -1))
       (reductions + 0)
       (take-while #(not (neg? %)))
       count))

(defn part-1 []
  (part-1* (slurp "input/2015/01")))

(defn part-2 []
  (part-2* (slurp "input/2015/01")))

(deftest test-part-1*
  (are [instructions floor] (= floor (part-1* instructions))
    "(())"     0          "()()"     0          "((("      3
    "(()(()("  3          "))((((("  3          "())"     -1
    "))("     -1          ")))"     -3          ")())())" -3))

(deftest test-part-2*
  (is (= 1 (part-2* ")")))
  (is (= 5 (part-2* "()())"))))

(deftest test-answers
  (is (= 138 (part-1)))
  (is (= 1771 (part-2))))
