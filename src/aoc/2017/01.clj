(ns aoc.2017.01
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (mapv #(- (int %) (int \0)) (str/trim s)))

(defn part-* [offset ds]
  (->> (range (count ds))
       (keep #(when (= (ds %) (ds (mod (+ offset %) (count ds))))
                (ds %)))
       (apply +)))

(defn part-1 [ds]
  (part-* 1 ds))

(defn part-2 [ds]
  (part-* (/ (count ds) 2) ds))

(deftest test-part-*
  (is (= 3 (part-* 1 [1 1 2 2])))
  (is (= 4 (part-* 1 [1 1 1 1])))
  (is (= 0 (part-* 1 [1 2 3 4])))
  (is (= 9 (part-* 1 [9 1 2 1 2 1 2 9])))
  (is (= 6 (part-* 2 [1 2 1 2])))
  (is (= 0 (part-* 2 [1 2 2 1])))
  (is (= 4 (part-* 3 [1 2 3 4 2 5])))
  (is (= 12 (part-* 3 [1 2 3 1 2 3])))
  (is (= 4 (part-* 4 [1 2 1 3 1 4 1 5]))))
