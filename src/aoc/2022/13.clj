(ns aoc.2022.13
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (read-string (str "[" s "]")))

(defn cmp [l r]
  (cond
    (and (int? l) (int? r)) (compare l r)
    (and (int? l) (vector? r)) (cmp (vector l) r)
    (and (vector? l) (int? r)) (cmp l (vector r))
    :else (if (or (empty? l) (empty? r))
            (compare l r)
            (let [c (cmp (first l) (first r))]
              (if (zero? c)
                (cmp (subvec l 1) (subvec r 1))
                c)))))

(defn part-1* [packets]
  (->> (partition 2 packets)
       (keep-indexed (fn [i [l r]] (when (neg? (cmp l r)) (inc i))))
       (apply +)))

(defn part-2* [packets]
  (let [sorted (sort cmp (conj packets [[2]] [[6]]))]
    (* (inc (.indexOf sorted [[2]])) (inc (.indexOf sorted [[6]])))))

(defn part-1 []
  (->> "input/2022/13" slurp parse part-1*))

(defn part-2 []
  (->> "input/2022/13" slurp parse part-2*))

(def example
  "[1,1,3,1,1] [1,1,5,1,1] [[1],[2,3,4]] [[1],4] [9] [[8,7,6]] [[4,4],4,4]
   [[4,4],4,4,4] [7,7,7,7] [7,7,7] [] [3] [[[]]] [[]]
   [1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9]")

(deftest test-example
  (is (= 13 (part-1* (parse example))))
  (is (= 140 (part-2* (parse example)))))
