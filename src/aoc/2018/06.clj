(ns aoc.2018.06
  (:require
   [aoc.vector :refer [manhattan-distance]]
   [clojure.test :refer [deftest is]]))

;; Like min-key, but returns nil if the minimum isn't unique.
(defn unique-min-key [f vs]
  (loop [min-k (f (first vs)) min-v (first vs) unique? true vs (rest vs)]
    (if-let [v (first vs)]
      (let [k (f v)]
        (cond
          (< k min-k) (recur k v true (rest vs))
          (= k min-k) (recur min-k min-v false (rest vs))
          :else (recur min-k min-v unique? (rest vs))))
      (when unique? min-v))))

(defn part-1* [points]
  (let [min-x (apply min (map first points))
        max-x (apply max (map first points))
        min-y (apply min (map second points))
        max-y (apply max (map second points))
        closest (fn [pos] (unique-min-key #(manhattan-distance pos %) points))]
    (->> (reduce
          (fn [point->area pos]
            (let [point (closest pos)]
              (if (contains? point->area point)
                (update point->area point inc)
                point->area)))
          ;; Points on the perimiter belong to infinite areas.
          (reduce
           (fn [point->area pos]
             (if-let [point (closest pos)]
               (dissoc point->area point)
               point->area))
           (into {} (map (fn [point] [point 0]) points))
           (concat
            (mapcat (fn [x] [[x min-y] [x max-y]]) (range min-x (inc max-x)))
            (mapcat (fn [y] [[min-x y] [max-x y]]) (range min-y (inc max-y)))))
          (for [x (range (inc min-x) max-x) y (range (inc min-y) max-y)] [x y]))
         vals (apply max))))

;; Would it be faster to start at the average of all the points, and then grow
;; the area from there?
(defn part-2* [max-dist points]
  (let [min-x (apply min (map first points))
        max-x (apply max (map first points))
        min-y (apply min (map second points))
        max-y (apply max (map second points))]
    (count
     (filter
      (fn [pos] (< (apply + (map #(manhattan-distance pos %) points)) max-dist))
      ;; The area could be bigger than this, but it isn't in this case.
      (for [x (range (inc min-x) max-x) y (range (inc min-y) max-y)] [x y])))))

(defn part-1 []
  (->> "input/2018/06" slurp (re-seq #"\d+") (map read-string) (partition 2)
       part-1*))

(defn part-2 []
  (->> "input/2018/06" slurp (re-seq #"\d+") (map read-string) (partition 2)
       (part-2* 10000)))

(deftest test-example
  (let [points [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]]]
    (is (= 17 (part-1* points)))
    (is (= 16 (part-2* 32 points)))))
