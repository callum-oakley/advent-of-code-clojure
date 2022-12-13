(ns aoc.2022.08
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (grid/parse s #(- (int %) (int \0))))

(defn visible? [trees [pos height]]
  (some (fn [dir]
          (->> pos (iterate #(+v % dir)) rest (map trees)
               (take-while identity) (every? #(< % height))))
        [grid/north grid/east grid/south grid/west]))

(defn scenic-score [trees [pos height]]
  (apply * (map (fn [dir]
                  (loop [pos (+v pos dir) visible 0]
                    (cond
                      (not (contains? trees pos)) visible
                      (< (trees pos) height) (recur (+v pos dir) (inc visible))
                      :else (inc visible))))
                [grid/north grid/east grid/south grid/west])))

(defn part-1 [trees]
  (count (filter #(visible? trees %) trees)))

(defn part-2 [trees]
  (apply max (map #(scenic-score trees %) trees)))

(deftest test-example
  (is (= 21 (part-1 (parse "30373\n25512\n65332\n33549\n35390"))))
  (is (= 8 (part-2 (parse "30373\n25512\n65332\n33549\n35390")))))
