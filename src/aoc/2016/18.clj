(ns aoc.2016.18
  (:require
   [clojure.test :refer [deftest is]]))

(defn step [tiles]
  (->> (concat [\.] tiles [\.])
       (partition 3 1)
       (map (fn [[left _ right]] (if (= left right) \. \^)))))

(defn part-* [n tiles]
  (->> (iterate step tiles) (take n) (apply concat) (filter #{\.}) count))

(defn part-1 [tiles]
  (part-* 40 tiles))

(defn part-2 [tiles]
  (part-* 400000 tiles))

(deftest test-part-*
  (is (= 38 (part-* 10 ".^^.^.^^^^"))))
