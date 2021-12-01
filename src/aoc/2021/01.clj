(ns aoc.2021.01
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn part-1* [xs]
  (->> xs (partition 2 1) (filter #(apply < %)) count))

(defn part-2* [xs]
  (->> xs (partition 3 1) (map #(apply + %)) part-1*))

(defn part-1 []
  (->> "input/2021/01" slurp str/split-lines (map read-string) part-1*))

(defn part-2 []
  (->> "input/2021/01" slurp str/split-lines (map read-string) part-2*))

(deftest test-example
  (is (= 7 (part-1* [199 200 208 210 200 207 240 269 260 263])))
  (is (= 5 (part-2* [199 200 208 210 200 207 240 269 260 263]))))
