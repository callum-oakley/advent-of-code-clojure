(ns aoc.2019.12
  (:require
   [aoc.vector :refer [transpose]]
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"-?\d+") (map parse-long) (partition 3)
       (map (fn [p] [p [0 0 0]]))))

(defn step [dim]
  (map
   (fn [[p v]]
     (let [v (apply + v (keep (fn [[q _]] (cond (< p q) +1 (< q p) -1)) dim))]
       [(+ p v) v]))
   dim))

(defn energy [[p v]]
  (* (apply + (map abs p)) (apply + (map abs v))))

(defn period [s]
  (inc (count (take-while #(not= (first s) %) (rest s)))))

(defn part-1* [steps moons]
  (->> moons (map transpose) transpose
       (map #(->> % (iterate step) (drop steps) first))
       transpose (map transpose) (map energy) (apply +)))

(defn part-2* [moons]
  (->> moons (map transpose) transpose
       (map #(period (iterate step %))) (reduce lcm)))

(defn part-1 []
  (->> "input/2019/12" slurp parse (part-1* 1000)))

(defn part-2 []
  (->> "input/2019/12" slurp parse part-2*))

(deftest test-example
  (is (= 179 (part-1* 10 (parse "-1 0 2 2 -10 -7 4 -8 8 3 5 -1"))))
  (is (= 1940 (part-1* 100 (parse "-8 -10 0 5 5 10 2 -7 3 9 -8 -3"))))
  (is (= 2772 (part-2* (parse "-1 0 2 2 -10 -7 4 -8 8 3 5 -1")))))
