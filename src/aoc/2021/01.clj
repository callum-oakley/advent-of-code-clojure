(ns aoc.2021.01
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map read-string (str/split-lines s)))

(defn part-1 [xs]
  (->> xs (partition 2 1) (filter #(apply < %)) count))

(defn part-2 [xs]
  (->> xs (partition 3 1) (map #(apply + %)) part-1))

(deftest test-example
  (is (= 7 (part-1 [199 200 208 210 200 207 240 269 260 263])))
  (is (= 5 (part-2 [199 200 208 210 200 207 240 269 260 263]))))
