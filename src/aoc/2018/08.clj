(ns aoc.2018.08
  (:require
   [clojure.test :refer [deftest is]]))

(defn parse [[child-count metadata-count & file]]
  (loop [children [] file file]
    (if (< (count children) child-count)
      (let [[child file] (parse file)] (recur (conj children child) file))
      [{:children children
        :metadata (take metadata-count file)}
       (drop metadata-count file)])))

(defn part-1* [tree]
  (apply + (apply + (:metadata tree)) (map part-1* (:children tree))))

(defn part-2* [tree]
  (apply +
         (if (seq (:children tree))
           (map #(part-2* (get (:children tree) (dec %))) (:metadata tree))
           (:metadata tree))))

(defn part-1 []
  (->> "input/2018/08" slurp (re-seq #"\d+") (map read-string) parse first
       part-1*))

(defn part-2 []
  (->> "input/2018/08" slurp (re-seq #"\d+") (map read-string) parse first
       part-2*))

(deftest test-example
  (let [tree (first (parse [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2]))]
    (is (= 138 (part-1* tree)))
    (is (= 66 (part-2* tree)))))
