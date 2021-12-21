(ns aoc.2021.21
  (:require
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"position: (\d)") (map second) (map read-string)))

(defn game-1 [[p1 p2]]
  (loop [dice (cycle (range 1 101)) rolls 0 p1 p1 p2 p2 s1 0 s2 0]
    (if (<= 1000 s2)
      (* s1 rolls)
      (let [p1 (inc (mod (dec (apply + p1 (take 3 dice))) 10))]
        (recur (drop 3 dice) (+ 3 rolls) p2 p1 s2 (+ p1 s1))))))

(def game-2
  (memoize
   (fn [p1 p2 s1 s2]
     (if (<= 21 s2)
       [0 1]
       (->> (for [a [1 2 3] b [1 2 3] c [1 2 3]] (+ a b c))
            (map #(let [p1 (inc (mod (dec (+ p1 %)) 10))]
                    (game-2 p2 p1 s2 (+ p1 s1))))
            (apply +v)
            reverse)))))

(defn part-1 []
  (->> "input/2021/21" slurp parse game-1))

(defn part-2 []
  (let [[p1 p2] (->> "input/2021/21" slurp parse)]
    (apply max (game-2 p1 p2 0 0))))

(deftest test-example
  (is (= 739785 (game-1 [4 8])))
  (is (= [444356092776315 341960390180808] (game-2 4 8 0 0))))
