(ns aoc.2021.07
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string)))

(defn find-min
  "Uses binary search to find the minimum of an integer function f between low
   and high. Works for sufficiently well behaved f."
  [f low high]
  (if (= 1 (- high low))
    (min (f low) (f high))
    (let [mid (quot (+ low high) 2)]
      ;; Is f increasing at mid?
      (if (< (f mid) (f (inc mid)))
        (recur f low mid)
        (recur f mid high)))))

(defn part-* [cost crabs]
  (find-min #(cost crabs %) (apply min crabs) (apply max crabs)))

(defn cost-1 [crabs x]
  (apply + (map #(abs (- % x)) crabs)))

(defn triangle [n]
  (/ (* n (inc n)) 2))

(defn cost-2 [crabs x]
  (apply + (map #(triangle (abs (- % x))) crabs)))

(defn part-1 [crabs]
  (part-* cost-1 crabs))

(defn part-2 [crabs]
  (part-* cost-2 crabs))

(deftest test-example
  (is (= 37 (part-1 (parse "16,1,2,0,4,2,7,1,2,14"))))
  (is (= 168 (part-2 (parse "16,1,2,0,4,2,7,1,2,14")))))
