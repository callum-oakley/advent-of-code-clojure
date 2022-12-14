(ns aoc.2022.14 
  (:require
   [aoc.vector :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]])
  (:import clojure.lang.PersistentQueue))

(defn parse [s]
  (->> (str/split-lines s)
       (map #(partition 2 (map read-string (re-seq #"\d+" %))))
       (reduce (fn [rock path]
                 (->> (partition 2 1 path)
                      (mapcat (fn [[[x0 y0] [x1 y1]]]
                                (let [[x0 x1] (sort [x0 x1])
                                      [y0 y1] (sort [y0 y1])]
                                  (for [x (range x0 (inc x1))
                                       y (range y0 (inc y1))]
                                   [y x]))))
                      (into rock)))
               #{})))

;; Adapted from 2018 day 17
(defn part-* [p rock]
  (let [max-y (apply max (map first rock))
        rock? (case p 1 rock 2 #(or (<= (+ max-y 2) (first %)) (rock %)))]
    (loop [flowing #{[0 500]}
           settled #{}
           queue (conj PersistentQueue/EMPTY [0 500])]
      (if-let [block (peek queue)]
        (let [down (+v block [1 0])
              left (+v block [1 -1])
              right (+v block [1 1])]
          (cond
            (and (= 1 p) (< max-y (first down)))
            (recur flowing settled (pop queue))

            (not (or (rock? down) (settled down) (flowing down)))
            (recur (conj flowing down) settled (conj (pop queue) down))

            (some flowing [left down right])
            (recur flowing settled (pop queue))

            (not (or (rock? left) (settled left)))
            (recur (conj flowing left) settled (conj (pop queue) left))

            (not (or (rock? right) (settled right)))
            (recur (conj flowing right) settled (conj (pop queue) right))

            :else
            (recur (disj flowing block)
                   (conj settled block)
                   (into (pop queue)
                         (filter flowing (map #(+v block [-1 %]) [-1 0 1]))))))
        (count settled)))))

(defn part-1 [rock]
  (part-* 1 rock))

(defn part-2 [rock]
  (part-* 2 rock))

(def example
  "498,4 -> 498,6 -> 496,6
   503,4 -> 502,4 -> 502,9 -> 494,9")

(deftest test-example
  (is (= 24 (part-1 (parse example))))
  (is (= 93 (part-2 (parse example)))))
