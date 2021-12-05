(ns aoc.2021.05
  (:require
   [aoc.vectors :refer [+v -v *v div-v chessboard-distance]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string) (partition 2) (partition 2)))

(defn hor-or-ver? [[[x0 y0] [x1 y1]]]
  (or (= x0 x1) (= y0 y1)))

(defn draw-line [[start end]]
  (let [len (chessboard-distance start end)
        dir (div-v (-v end start) len)]
    (->> (range (inc len)) (map #(+v start (*v % dir))))))

(defn part-* [p lines]
  (->> (case p 1 (filter hor-or-ver? lines) 2 lines)
       (mapcat draw-line) frequencies vals (filter #(<= 2 %)) count))

(defn part-1 []
  (->> "input/2021/05" slurp parse (part-* 1)))

(defn part-2 []
  (->> "input/2021/05" slurp parse (part-* 2)))

(deftest test-draw-line
  (is (= [[1 1] [1 2] [1 3]] (draw-line [[1 1] [1 3]])))
  (is (= [[9 7] [8 7] [7 7]] (draw-line [[9 7] [7 7]])))
  (is (= [[1 1] [2 2] [3 3]] (draw-line [[1 1] [3 3]])))
  (is (= [[9 7] [8 8] [7 9]] (draw-line [[9 7] [7 9]]))))

(deftest test-example
  (let [lines (parse "0,9 -> 5,9 8,0 -> 0,8 9,4 -> 3,4 2,2 -> 2,1 7,0 -> 7,4
                      6,4 -> 2,0 0,9 -> 2,9 3,4 -> 1,4 0,0 -> 8,8 5,5 -> 8,2")]
    (is (= 5 (part-* 1 lines)))
    (is (= 12 (part-* 2 lines)))))
