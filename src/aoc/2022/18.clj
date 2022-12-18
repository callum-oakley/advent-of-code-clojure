(ns aoc.2022.18
  (:require
   [aoc.grid :as grid]
   [aoc.search :as search]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"-?\d+") (map read-string) (partition 3) (map vec) set))

(defn part-1 [lava]
  (count (remove lava (mapcat grid/adjacent-3d lava))))

(defn part-2 [lava]
  (let [[[min-x max-x] [min-y max-y] [min-z max-z]] (grid/box lava)
        exterior (set (search/dft
                       [(dec min-x) (dec min-y) (dec min-z)]
                       #(filter (fn [[x y z]]
                                  (and (not (lava [x y z]))
                                       (<= (dec min-x) x (inc max-x))
                                       (<= (dec min-y) y (inc max-y))
                                       (<= (dec min-z) z (inc max-z))))
                                (grid/adjacent-3d %))
                       identity))]
    (count (filter exterior (mapcat grid/adjacent-3d lava)))))

(def example
  "2,2,2 1,2,2 3,2,2 2,1,2 2,3,2 2,2,1 2,2,3
   2,2,4 2,2,6 1,2,5 3,2,5 2,1,5 2,3,5")

(deftest test-example
  (is (= 64 (part-1 (parse example))))
  (is (= 58 (part-2 (parse example)))))
