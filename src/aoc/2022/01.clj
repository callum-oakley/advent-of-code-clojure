(ns aoc.2022.01
  (:require
   [clojure.string :as str]))

(defn parse [s]
  (map #(map read-string (str/split-lines %)) (str/split s #"\n\n")))

(defn part-1 [elves]
  (->> elves (map #(apply + %)) (apply max)))

(defn part-2 [elves]
  (->> elves (map #(apply + %)) (sort >) (take 3) (apply +)))
