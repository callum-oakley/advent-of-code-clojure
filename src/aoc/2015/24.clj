(ns aoc.2015.24
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn part-* [parts min-group-size packages]
  (let [target (/ (apply + packages) parts)]
    ;; Strictly speaking we should ensure the remaining packages can also be
    ;; split. Turns out they can but checking is slow.
    (if-let [groups (seq (filter #(= target (apply + %))
                                 (comb/combinations packages min-group-size)))]
      (apply min (map #(apply * %) groups))
      (recur parts (inc min-group-size) packages))))

(defn part-1 []
  (->> "input/2015/24" slurp str/split-lines (map read-string) (part-* 3 1)))

(defn part-2 []
  (->> "input/2015/24" slurp str/split-lines (map read-string) (part-* 4 1)))

(deftest test-part-*
  (is (= 99 (part-* 3 1 [1 2 3 4 5 7 8 9 10 11])))
  (is (= 44 (part-* 4 1 [1 2 3 4 5 7 8 9 10 11]))))
