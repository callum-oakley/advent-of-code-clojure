(ns aoc.2018.09
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map read-string (re-seq #"\d+" s)))

(defn part-1 [[players max-marble]]
  (loop [left (transient (vec (repeat (inc max-marble) 0)))
         right (transient (vec (repeat (inc max-marble) 0)))
         current 0
         next 1
         scores (vec (repeat players 0))]
    (cond
      (< max-marble next)
      (apply max scores)

      (zero? (mod next 23))
      (let [a ((apply comp (repeat 6 left)) current) b (left a) c (left b)]
        (recur (assoc! left a c)
               (assoc! right c a)
               a
               (inc next)
               (update scores (mod next players) + next b)))

      :else
      (let [a (right current) b (right a)]
        (recur (assoc! left b next next a)
               (assoc! right a next next b)
               next
               (inc next)
               scores)))))

(defn part-2 [[players max-marble]]
  (part-1 [players (* 100 max-marble)]))

(deftest test-examples
  (is (= 32 (part-1 [9 25])))
  (is (= 8317 (part-1 [10 1618])))
  (is (= 146373 (part-1 [13 7999])))
  (is (= 2764 (part-1 [17 1104])))
  (is (= 54718 (part-1 [21 6111])))
  (is (= 37305 (part-1 [30 5807]))))
