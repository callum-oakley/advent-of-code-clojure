(ns aoc.2022.04
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string) (partition 2) (partition 2)))

(defn full-overlap? [[[a b] [c d]]]
  (or (<= a c d b) (<= c a b d)))

(defn partial-overlap? [[[a b] [c d]]]
  (or (<= a c b) (<= a d b)))

(defn part-1 [pairs]
  (count (filter full-overlap? pairs)))

(defn part-2 [pairs]
  (count (filter (some-fn full-overlap? partial-overlap?) pairs)))

(def example
  "2-4,6-8 2-3,4-5 5-7,7-9 2-8,3-7 6-6,4-6 2-6,4-8")

(deftest test-example
  (is (= 2 (part-1 (parse example))))
  (is (= 4 (part-2 (parse example)))))
