(ns aoc.2019.24
  (:require
   [aoc.grid :as g]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s g/parse (keep (fn [[k v]] (when (= \# v) k))) set))

(defn tick [adjacent bugs]
  (->> (mapcat adjacent bugs)
       (into bugs)
       (filter #(if (bugs %)
                  (= 1 (count (filter bugs (adjacent %))))
                  (<= 1 (count (filter bugs (adjacent %))) 2)))
       set))

(defn adjacent-1 [pos]
  (filter (fn [[y x]] (and (<= 0 y 4) (<= 0 x 4))) (g/adjacent pos)))

(defn adjacent-2 [[z y x]]
  (cond-> (->> [y x] adjacent-1 (remove #{[2 2]}) (map (fn [[y x]] [z y x])))
    (= 0 y) (conj [(dec z) 1 2])
    (= 0 x) (conj [(dec z) 2 1])
    (= 4 y) (conj [(dec z) 3 2])
    (= 4 x) (conj [(dec z) 2 3])
    (= [1 2] [y x]) (concat (map (fn [x] [(inc z) 0 x]) (range 5)))
    (= [3 2] [y x]) (concat (map (fn [x] [(inc z) 4 x]) (range 5)))
    (= [2 1] [y x]) (concat (map (fn [y] [(inc z) y 0]) (range 5)))
    (= [2 3] [y x]) (concat (map (fn [y] [(inc z) y 4]) (range 5)))))

(defn biodiversity [bugs]
  (->> (for [y (range 5) x (range 5)] [y x])
       (filter bugs)
       (map (fn [[y x]] (bit-shift-left 1 (+ (* 5 y) x))))
       (apply +)))

(defn part-1* [bugs]
  (loop [bugs bugs seen #{}]
    (if (seen bugs)
      (biodiversity bugs)
      (recur (tick adjacent-1 bugs) (conj seen bugs)))))

(defn part-2* [ticks bugs]
  (->> bugs (map (fn [[y x]] [0 y x])) set (iterate #(tick adjacent-2 %))
       (drop ticks) first count))

(defn part-1 []
  (->> "input/2019/24" slurp parse part-1*))

(defn part-2 []
  (->> "input/2019/24" slurp parse (part-2* 200)))

(deftest test-examples
  (is (= 2129920 (part-1* (parse "....#\n#..#.\n#..##\n..#..\n#...."))))
  (is (= 99 (part-2* 10 (parse "....#\n#..#.\n#..##\n..#..\n#....")))))
