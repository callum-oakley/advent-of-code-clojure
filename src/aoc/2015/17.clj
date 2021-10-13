(ns aoc.2015.17
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn fills [eggnog [c & cs]]
  (cond
    (zero? eggnog) [[]]
    (or (neg? eggnog) (nil? c)) []
    :else (concat (map #(conj % c) (fills (- eggnog c) cs))
                  (fills eggnog cs))))

(defn part-* []
  (->> "input/2015/17"
       slurp
       str/split-lines
       (map read-string)
       (fills 150)))

(defn part-1 []
  (count (part-*)))

(defn part-2 []
  (let [count->fills (group-by count (part-*))]
    (count (count->fills (apply min (keys count->fills))))))

(deftest test-fills
  (is (= 4 (count (fills 25 [20 15 10 5 5])))))

(deftest test-answers
  (is (= 654 (part-1)))
  (is (= 57 (part-2))))
