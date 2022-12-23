(ns aoc.2019.15
  (:require
   [aoc.2019.intcode :as i]
   [aoc.grid :as g]
   [aoc.search :as search]
   [aoc.vector :refer [+v]]))

(def parse i/parse)

(defn adjacent [{:keys [vm pos steps]}]
  (keep (fn [in]
          (let [[out vm] (i/io vm [in])
                pos (+v (case in 1 g/north 2 g/south 3 g/west 4 g/east) pos)]
            (case out
              [0] nil
              [1] {:vm vm :pos pos :steps (inc steps)}
              [2] {:vm vm :pos pos :steps (inc steps) :done? true})))
        [1 2 3 4]))

(defn part-1* [mem]
  (search/bfs {:vm (i/run mem) :pos [0 0] :steps 0} adjacent :pos :done?))

(defn part-1 [mem]
  (:steps (part-1* mem)))

(defn part-2 [mem]
  (->> (search/bft (assoc (part-1* mem) :steps 0) adjacent :pos)
       (map :steps) (apply max)))
