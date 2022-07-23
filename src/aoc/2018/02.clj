(ns aoc.2018.02
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]))

(defn part-1 []
  (let [fs (->> "input/2018/02" slurp str/split-lines
                (map #(set (vals (frequencies %)))))]
    (* (count (filter #(% 2) fs)) (count (filter #(% 3) fs)))))

(defn match [a b]
  (let [c (->> (map vector a b) (filter #(apply = %)) (map first) str/join)]
    (when (= (count c) (dec (count a))) c)))

(defn part-2 []
  (some #(apply match %)
        (comb/combinations (str/split-lines (slurp "input/2018/02")) 2)))
