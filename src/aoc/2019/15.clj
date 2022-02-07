(ns aoc.2019.15
  (:require
   [aoc.2019.intcode :as i]
   [aoc.grid :as g]
   [aoc.search :as search]
   [aoc.vector :refer [+v]]))

(defn adjacent [{:keys [vm pos steps]}]
  (keep (fn [in]
          (let [[out vm] (i/io vm [in])
                pos (+v (case in 1 g/north 2 g/south 3 g/west 4 g/east) pos)]
            (case out
              [0] nil
              [1] {:vm vm :pos pos :steps (inc steps)}
              [2] {:vm vm :pos pos :steps (inc steps) :done? true})))
        [1 2 3 4]))

(defn part-1* []
  (search/bfs {:vm (i/run (i/load "input/2019/15")) :pos [0 0] :steps 0}
              adjacent :done? :pos))

(defn part-1 []
  (:steps (part-1*)))

(defn part-2 []
  (->> (search/bft (assoc (part-1*) :steps 0) adjacent :pos)
       (map :steps) (apply max)))
