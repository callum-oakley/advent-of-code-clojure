(ns aoc.2018.23
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [manhattan-distance +v]]
   [aoc.search :as search]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map (fn [[x y z r]] {:pos [z y x] :r r})
       (partition 4 (map read-string (re-seq #"-?\d+" s)))))

(defn part-1 [bots]
  (let [{:keys [pos r]} (apply max-key :r bots)]
    (count (filter #(<= (manhattan-distance pos (:pos %)) r) bots))))

;; Does the given box intersect with the given bot's range?
(defn intersects? [{box :pos w :w} {bot :pos r :r}]
  (<= (apply + (map #(let [a (bot %) low (box %) high (+ (box %) w -1)]
                       (cond (< a low) (- low a) (< high a) (- a high) :else 0))
                    (range 3)))
      r))

;; We can hammer this puzzle in to a dijkstra shaped hole. Start with a box
;; containing every bot. We can take a "path" to a single coordinate by
;; repeatedly splitting this box in to 8 smaller boxes of half the width, and
;; choosing one. Let the "cost" of a box be the number of bots which DON'T
;; intersect with it, then this cost always increases along any path, and the
;; problem reduces to finding a lowest cost path to a box of width 1.
(defn part-2 [bots]
  (let [[[z0 z1] [y0 y1] [x0 x1]] (grid/box (map :pos bots))
        min-w (inc (max (- z1 z0) (- y1 y0) (- x1 x0)))] 
    (manhattan-distance
     (:pos
      (search/dijkstra
       ;; Choose a starting box with a power of 2 width for easy division
       {:pos [z0 y0 x0] :w (loop [w 1] (if (< w min-w) (recur (* w 2)) w))}
       (fn [{:keys [pos w]}]
         (for [z [0 (/ w 2)] y [0 (/ w 2)] x [0 (/ w 2)]]
           {:pos (+v pos [z y x]) :w (/ w 2)}))
       identity
       #(= (:w %) 1)
       (fn [box]
         [(count (remove #(intersects? box %) bots))
          ;; Tie break by manhattan distance
          (manhattan-distance (:pos box))]))))))

(deftest test-example
  (is (= 7 (part-1 (parse "0 0 0 4 1 0 0 1 4 0 0 3 0 2 0 1 0 5 0 3
                           0 0 3 1 1 1 1 1 1 1 2 1 1 3 1 1"))))
  (is (= 36 (part-2 (parse "10 12 12 2 12 14 12 2 16 12 12 4 14 14 14 6
                            50 50 50 200 10 10 10 5")))))
