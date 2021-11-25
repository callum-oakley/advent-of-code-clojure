(ns aoc.2016.18
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn step [tiles]
  (->> (concat [\.] tiles [\.])
       (partition 3 1)
       (map (fn [[left _ right]] (if (= left right) \. \^)))))

(defn part-* [n tiles]
  (->> (iterate step tiles) (take n) (apply concat) (filter #{\.}) count))

(defn part-1 []
  (part-* 40 (str/trim (slurp "input/2016/18"))))

(defn part-2 []
  (part-* 400000 (str/trim (slurp "input/2016/18"))))

(deftest test-part-*
  (is (= 38 (part-* 10 ".^^.^.^^^^"))))
