(ns aoc.2015.17
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map read-string (str/split-lines s)))

(defn fills [eggnog [c & cs]]
  (cond
    (zero? eggnog) [[]]
    (or (neg? eggnog) (nil? c)) []
    :else (concat (map #(conj % c) (fills (- eggnog c) cs))
                  (fills eggnog cs))))

(defn part-1 [containers]
  (count (fills 150 containers)))

(defn part-2 [containers]
  (let [count->fills (group-by count (fills 150 containers))]
    (count (count->fills (apply min (keys count->fills))))))

(deftest test-fills
  (is (= 4 (count (fills 25 [20 15 10 5 5])))))
