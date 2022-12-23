(ns aoc.2019.04
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string)))

(defn valid? [f pass]
  (let [digits (map int (str pass))]
    (and (apply <= digits)
         (some #(f 2 (count %)) (partition-by identity digits)))))

(defn part-* [input f]
  (->> input (apply range) (filter #(valid? f %)) count))

(defn part-1 [input]
  (part-* input <=))

(defn part-2 [input]
  (part-* input =))

(deftest test-example
  (is (valid? <= 111111))
  (is (not (valid? <= 223450)))
  (is (not (valid? <= 123789)))
  (is (valid? = 112233))
  (is (not (valid? = 123444)))
  (is (valid? = 111122)))
