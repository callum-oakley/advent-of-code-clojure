(ns aoc.2019.11
  (:require
   [aoc.2019.intcode :as i]
   [aoc.grid :as grid]
   [aoc.ocr :as ocr]
   [aoc.vector :refer [+v]]))

(def parse i/parse)

(defn paint [mem hull]
  (loop [vm (i/run mem) pos [0 0] dir grid/north hull hull]
    (if (= :halt (:state vm))
      hull
      (let [[out vm] (i/io vm [(get hull pos 0)])
            dir ((case (second out) 0 grid/left 1 grid/right) dir)]
        (recur vm (+v pos dir) dir (assoc hull pos (first out)))))))

(defn part-1 [mem]
  (count (paint mem {})))

(defn part-2 [mem]
  (->> {[0 0] 1} (paint mem) (filter (comp #{1} val)) (map (comp reverse key))
       set ocr/draw ocr/parse))
