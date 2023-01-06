(ns aoc.2015.24
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map read-string (str/split-lines s)))

(defn part-* [parts min-group-size packages]
  (let [target (/ (apply + packages) parts)]
    ;; Strictly speaking we should ensure the remaining packages can also be
    ;; split. Turns out they can but checking is slow.
    (if-let [groups (seq (filter #(= target (apply + %))
                                 (comb/combinations packages min-group-size)))]
      (apply min (map #(apply * %) groups))
      (recur parts (inc min-group-size) packages))))

(defn part-1 [packages]
  (part-* 3 1 packages))

(defn part-2 [packages]
  (part-* 4 1 packages))

(deftest test-examples
  (is (= 99 (part-1 [1 2 3 4 5 7 8 9 10 11])))
  (is (= 44 (part-2 [1 2 3 4 5 7 8 9 10 11]))))
