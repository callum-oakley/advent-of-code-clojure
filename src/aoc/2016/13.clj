(ns aoc.2016.13
  (:require
   [aoc.search :as search]
   [aoc.vectors :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn wall? [n [x y]]
  (or (neg? x) (neg? y)
      (odd? (Integer/bitCount (+ n (* x x) (* 3 x) (* 2 x y) y (* y y))))))

(defn adjacent [n state]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (map (fn [dir] (-> state (update :pos +v dir) (update :steps inc))))
       (remove #(wall? n (:pos %)))))

(defn part-1* [n target]
  (:steps (search/bfs {:pos [1 1] :steps 0}
                      #(adjacent n %)
                      #(= target (:pos %))
                      :pos)))

(defn part-1 []
  (part-1* (read-string (slurp "input/2016/13")) [31 39]))

(defn part-2 []
  (let [n (read-string (slurp "input/2016/13"))
        c (atom 0)]
    (search/bfs {:pos [1 1] :steps 0}
                #(do (swap! c inc) (adjacent n %))
                #(< 50 (:steps %))
                :pos)
    @c))

(deftest test-sample
  (is (= 11 (part-1* 10 [7 4]))))

(deftest test-answers
  (is (= 90 (part-1)))
  (is (= 135 (part-2))))
