(ns aoc.2021.09
  (:require
   [aoc.grid :as grid]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (grid/parse s #(- (int %) (int \0))))

(defn low-points [floor]
  (->> floor
       (filter (fn [[pos height]]
                 (every? #(< height (floor %)) (grid/adjacent pos floor))))
       (map key)
       set))

(defn part-1* [floor]
  (->> floor low-points (map floor) (map inc) (apply +)))

;; Identify basins with their low point, so for a given position, return the
;; low point of the basin it's in.
(defn basin-fn [floor low-point?]
  (let [go (memoize
            (fn [k pos]
              (cond
                (= 9 (floor pos)) nil
                (low-point? pos) pos
                :else (k k (apply min-key floor (grid/adjacent pos floor))))))]
    (fn [pos] (go go pos))))

(defn part-2* [floor]
  (let [low-point? (low-points floor)]
    (->> floor keys (keep (basin-fn floor low-point?))
         frequencies vals sort (take-last 3) (apply *))))

(defn part-1 []
  (->> "input/2021/09" slurp parse part-1*))

(defn part-2 []
  (->> "input/2021/09" slurp parse part-2*))

(deftest test-example
  (let [sample "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"]
    (is (= 15 (part-1* (parse sample))))
    (is (= 1134 (part-2* (parse sample))))))
