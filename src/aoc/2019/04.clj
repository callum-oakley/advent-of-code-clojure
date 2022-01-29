(ns aoc.2019.04
  (:require
   [clojure.test :refer [deftest is]]))

(defn valid? [f pass]
  (let [digits (map int (str pass))]
    (and (apply <= digits)
         (some #(f 2 (count %)) (partition-by identity digits)))))

(defn part-* [f]
  (->> "input/2019/04" slurp (re-seq #"\d+") (map parse-long) (apply range)
       (filter #(valid? f %)) count))

(defn part-1 []
  (part-* <=))

(defn part-2 []
  (part-* =))

(deftest test-example
  (is (valid? <= 111111))
  (is (not (valid? <= 223450)))
  (is (not (valid? <= 123789)))
  (is (valid? = 112233))
  (is (not (valid? = 123444)))
  (is (valid? = 111122)))
