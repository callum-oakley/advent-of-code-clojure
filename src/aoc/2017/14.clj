(ns aoc.2017.14
  (:require
   [aoc.2017.10 :refer [knot-hash]]
   [aoc.hash :as hash]
   [aoc.search :as search]
   [aoc.vectors :refer [+v]]
   [clojure.test :refer [deftest is]]))

(defn disk [seed]
  (mapv #(vec (hash/bits (knot-hash (str seed "-" %)))) (range 128)))

(defn part-1* [seed]
  (->> seed disk flatten (filter identity) count))

(defn part-2* [seed]
  (let [disk (disk seed)
        adjacent (fn [a]
                   (->> [[0 1] [0 -1] [1 0] [-1 0]]
                        (map #(+v a %))
                        (filter #(get-in disk %))))]
    (loop [regions 0
           unexplored (set (for [x (range 128) y (range 128)] [x y]))]
      (if-let [a (first unexplored)]
        (if (get-in disk a)
          (recur (inc regions)
                 (apply disj unexplored (search/dft a adjacent)))
          (recur regions
                 (disj unexplored a)))
        regions))))

(defn part-1 []
  (->> "input/2017/14" slurp part-1*))

(defn part-2 []
  (->> "input/2017/14" slurp part-2*))

(deftest test-example
  (is (= 8108 (part-1* "flqrgnkx")))
  (is (= 1242 (part-2* "flqrgnkx"))))

(deftest test-answers
  (is (= 8106 (part-1)))
  (is (= 1164 (part-2))))
