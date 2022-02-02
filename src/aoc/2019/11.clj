(ns aoc.2019.11
  (:require
   [aoc.2019.intcode :as i]
   [aoc.grid :as grid]
   [aoc.ocr :as ocr]
   [aoc.vector :refer [+v]]))

(defn paint [hull]
  (loop [vm (i/run (i/load "input/2019/11")) pos [0 0] dir grid/north hull hull]
    (if (= :halt (:state vm))
      hull
      (let [vm (i/io vm [(get hull pos 0)])
            dir ((case (second (:out vm)) 0 grid/left 1 grid/right) dir)]
        (recur vm (+v pos dir) dir (assoc hull pos (first (:out vm))))))))

(defn part-1 []
  (count (paint {})))

(defn part-2 []
  (->> {[0 0] 1} paint (filter (comp #{1} val)) (map (comp reverse key)) set
       ocr/draw ocr/parse))
