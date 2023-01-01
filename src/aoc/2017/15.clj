(ns aoc.2017.15
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map read-string (re-seq #"\d+" s)))

(defn generate [seed factor]
  (rest (iterate #(mod (* % factor) 2147483647) seed)))

(defn judge [n as bs]
  (->> (map (fn [a b] (= (bit-and a 0xffff) (bit-and b 0xffff))) as bs)
       (take n) (filter identity) count))

(defn part-1 [[seed-a seed-b]]
  (judge 40000000 (generate seed-a 16807) (generate seed-b 48271)))

(defn part-2 [[seed-a seed-b]]
  (judge 5000000
         (filter #(zero? (mod % 4)) (generate seed-a 16807))
         (filter #(zero? (mod % 8)) (generate seed-b 48271))))

(deftest test-example
  (is (= 588 (part-1 [65 8921])))
  (is (= 309 (part-2 [65 8921]))))
