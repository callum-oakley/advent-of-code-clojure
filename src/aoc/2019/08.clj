(ns aoc.2019.08
  (:require
   [aoc.ocr :as ocr]
   [aoc.vector :refer [transpose]]
   [clojure.string :as str]))

(defn parse [s]
  (->> s (re-seq #"\d") (map parse-long) (partition 150)))

(defn part-1 []
  (->> "input/2019/08" slurp parse (map frequencies) (apply min-key #(% 0))
       (#(* (% 1) (% 2)))))

(defn part-2 []
  (->> "input/2019/08" slurp parse transpose
       (map #(reduce (fn [x y] (if (= 2 x) y x)) %)) (map {0 \. 1 \#})
       (partition 25) (map #(apply str %)) (str/join "\n") ocr/parse))
