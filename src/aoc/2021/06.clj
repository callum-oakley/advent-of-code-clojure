(ns aoc.2021.06
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [f (->> s (re-seq #"\d+") (map read-string) frequencies)]
    (mapv #(get f % 0) (range 9))))

(defn step [fish]
  (conj (update (subvec fish 1) 6 + (fish 0)) (fish 0)))

(defn part-* [days fish]
  (->> fish (iterate step) (drop days) first (apply +)))

(defn part-1 []
  (->> "input/2021/06" slurp parse (part-* 80)))

(defn part-2 []
  (->> "input/2021/06" slurp parse (part-* 256)))

(deftest test-example
  (let [fish (parse "3,4,3,1,2")]
    (is (= 26 (part-* 18 fish)))
    (is (= 5934 (part-* 80 fish)))
    (is (= 26984457539 (part-* 256 fish)))))
