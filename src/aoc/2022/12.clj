(ns aoc.2022.12 
  (:require
   [aoc.grid :as grid]
   [aoc.search :as search]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [g (grid/parse s)]
    [(update-vals g #(int (case % \S \a \E \z %)))
     (->> g keys (filter #(= \S (g %))) first)
     (->> g keys (filter #(= \E (g %))) first)
     (->> g keys (filter #(#{\a \S} (g %))) set)]))

;; If we do the BFS in reverse (from E) then part 2 is no more difficult than
;; part 1, we just have multiple goals to consider.
(defn part-* [g goal start?]
  (:steps
   (search/bfs {:pos goal :steps 0}
               (fn [state]
                 (->> (grid/adjacent (:pos state) g)
                      (filter #(<= (g (:pos state)) (inc (g %))))
                      (map (fn [pos] {:pos pos :steps (inc (:steps state))}))))
               #(start? (:pos %))
               :pos)))

(defn part-1 []
  (let [[g start goal _] (parse (slurp "input/2022/12"))]
    (part-* g goal #{start})))

(defn part-2 []
  (let [[g _ goal low-points] (parse (slurp "input/2022/12"))]
    (part-* g goal low-points)))

(def example
  "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")

(deftest test-example
  (let [[g start goal low-points] (parse example)]
    (is (= 31 (part-* g goal #{start})))
    (is (= 29 (part-* g goal low-points)))))