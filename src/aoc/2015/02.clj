(ns aoc.2015.02
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn paper [box]
  (let [[a b c] (sort (map read-string (str/split box #"x")))]
    (+ (* 3 a b) (* 2 b c) (* 2 c a))))

(defn ribbon [box]
  (let [[a b c] (sort (map read-string (str/split box #"x")))]
    (+ a a b b (* a b c))))

(defn part-1 []
  (->> (slurp "input/2015/02") str/split-lines (map paper) (apply +)))

(defn part-2 []
  (->> (slurp "input/2015/02") str/split-lines (map ribbon) (apply +)))

(deftest test-paper
  (is (= 58 (paper "2x3x4")))
  (is (= 43 (paper "1x1x10"))))

(deftest test-ribbon
  (is (= 34 (ribbon "2x3x4")))
  (is (= 14 (ribbon "1x1x10"))))
