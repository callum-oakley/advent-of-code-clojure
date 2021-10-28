(ns aoc.2016.19
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn part-1* [n]
  (loop [size n
         top 1
         left (transient
               (into {} (map (fn [i] [(if (zero? i) n i) (inc i)])) (range n)))]
    (if (= 1 size)
      top
      (recur (dec size)
             (left (left top))
             (assoc! left top (left (left top)))))))

(defn part-2* [n]
  (loop [size n
         top 1
         bottom (quot n 2)
         left (transient
               (into {} (map (fn [i] [(if (zero? i) n i) (inc i)])) (range n)))]
    (if (= 1 size)
      top
      (let [left (assoc! left bottom (left (left bottom)))]
        (recur (dec size) (left top) (cond-> bottom (odd? size) left) left)))))

(defn part-1 []
  (part-1* (read-string (slurp "input/2016/19"))))

(defn part-2 []
  (part-2* (read-string (slurp "input/2016/19"))))

(deftest test-examples
  (is (= 3 (part-1* 5)))
  (is (= 2 (part-2* 5))))

(deftest test-answers
  (is (= 1841611 (part-1)))
  (is (= 1423634 (part-2))))
