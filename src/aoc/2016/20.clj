(ns aoc.2016.20
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s str/split-lines (map #(mapv read-string (re-seq #"\d+" %)))))

(defn part-* [limit blacklist]
  (let [[allowed i]
        (reduce (fn [[allowed i] [low high]]
                  [(into allowed (range i low)) (max i (inc high))])
                [[] 0]
                (sort blacklist))]
    (into allowed (range i (inc limit)))))

(defn part-1 []
  (first (part-* 4294967295 (parse (slurp "input/2016/20")))))

(defn part-2 []
  (count (part-* 4294967295 (parse (slurp "input/2016/20")))))

(deftest test-sample
  (is (= [3 9] (part-* 9 [[5 8] [0 2] [4 7]]))))
