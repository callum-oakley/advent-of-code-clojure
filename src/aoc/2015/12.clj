(ns aoc.2015.12
  (:require
   [clojure.data.json :as json]
   [clojure.test :refer [deftest is are]]))

(defn part-* [blacklist? v]
  (cond
    (number? v) v
    (string? v) 0
    (vector? v) (->> v (map #(part-* blacklist? %)) (apply +))
    (map? v) (if (some blacklist? (vals v))
               0
               (->> v vals (map #(part-* blacklist? %)) (apply +)))))

(defn part-1 []
  (->> "input/2015/12" slurp json/read-str (part-* #{})))

(defn part-2 []
  (->> "input/2015/12" slurp json/read-str (part-* #{"red"})))

(deftest test-part-*
  (are [v s] (= v (part-* #{} (json/read-str s)))
    6 "[1,2,3]"          6 "{\"a\":2,\"b\":4}"
    3 "[[[3]]]"          3 "{\"a\":{\"b\":4},\"c\":-1}"
    0 "{\"a\":[-1,1]}"   0 "[-1,{\"a\":1}]"
    0 "[]"               0 "{}")
  (are [v s] (= v (part-* #{"red"} (json/read-str s)))
    6 "[1,2,3]"
    4 "[1,{\"c\":\"red\",\"b\":2},3]"
    0 "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"
    6 "[1,\"red\",5]"))

(deftest test-answers
  (is (= 119433 (part-1)))
  (is (= 68466 (part-2))))
