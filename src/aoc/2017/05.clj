(ns aoc.2017.05
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn part-* [p jumps]
  (loop [steps 0 head 0 jumps jumps]
    (if (contains? jumps head)
      (recur (inc steps)
             (+ (jumps head) head)
             (update jumps head (case p
                                  1 inc
                                  2 #(if (< % 3) (inc %) (dec %)))))
      steps)))

(defn part-1 []
  (->> "input/2017/05" slurp str/split-lines (mapv read-string) (part-* 1)))

(defn part-2 []
  (->> "input/2017/05" slurp str/split-lines (mapv read-string) (part-* 2)))

(deftest test-examples
  (is (= 5 (part-* 1 [0 3 0 1 -3])))
  (is (= 10 (part-* 2 [0 3 0 1 -3]))))
