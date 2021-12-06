(ns aoc.2021.06
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [f (->> s (re-seq #"\d+") (map read-string) frequencies)]
    (mapv #(get f % 0) (range 9))))

(defn simulate [fish]
  (iterate #(conj (update (subvec % 1) 6 + (% 0)) (% 0)) fish))

(defn part-* [days fish]
  (apply + (nth (simulate fish) days)))

(defn part-1 []
  (->> "input/2021/06" slurp parse (part-* 80)))

(defn part-2 []
  (->> "input/2021/06" slurp parse (part-* 256)))

(deftest test-example
  (let [fish (parse "3,4,3,1,2")]
    (= 26 (part-* 18 fish))
    (= 5934 (part-* 80 fish))
    (= 26984457539 (part-* 256 fish))))
