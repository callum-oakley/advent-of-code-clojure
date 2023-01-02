(ns aoc.2016.13
  (:require
   [aoc.search :as search]
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

(def parse read-string)

(defn wall? [n [x y]]
  (or (neg? x) (neg? y)
      (odd? (Integer/bitCount (+ n (* x x) (* 3 x) (* 2 x y) y (* y y))))))

(defn adjacent [n step-limit state]
  (when (not= step-limit (:steps state))
    (->> [[1 0] [-1 0] [0 1] [0 -1]]
         (map (fn [dir] (-> state (update :pos +v dir) (update :steps inc))))
         (remove #(wall? n (:pos %))))))

(defn part-1* [n target]
  (:steps (search/bfs {:pos [1 1] :steps 0}
                      #(adjacent n nil %)
                      :pos
                      #(= target (:pos %)))))

(defn part-1 [n]
  (part-1* n [31 39]))

(defn part-2 [n]
  (count (search/bft {:pos [1 1] :steps 0} #(adjacent n 50 %) :pos)))

(deftest test-sample
  (is (= 11 (part-1* 10 [7 4]))))
