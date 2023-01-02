(ns aoc.2016.15
  (:require
   [aoc.number-theory :as number-theory]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map #(let [[disc positions _ start] (map read-string (re-seq #"\d+" %))]
          [disc positions start])
       (str/split-lines s)))

(defn part-1 [discs]
  (number-theory/chinese-remainder-theorem
   (map (fn [[disc positions start]]
          ;; Want time so that start + disc + time = 0 (mod positions)
          ;; => time = 0 - disc - start (mod positions)
          [(mod (- 0 disc start) positions) positions])
        discs)))

(defn part-2 [discs]
  (part-1 (conj discs [7 11 0])))

(deftest test-part-*
  (is (= 5 (part-1 [[1 5 4] [2 2 1]]))))
