(ns aoc.2018.02
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]))

(def parse str/split-lines)

(defn part-1 [ids]
  (let [fs (map #(set (vals (frequencies %))) ids)]
    (* (count (filter #(% 2) fs)) (count (filter #(% 3) fs)))))

(defn match [a b]
  (let [c (->> (map vector a b) (filter #(apply = %)) (map first) str/join)]
    (when (= (count c) (dec (count a))) c)))

(defn part-2 [ids]
  (some #(apply match %) (comb/combinations ids 2)))
