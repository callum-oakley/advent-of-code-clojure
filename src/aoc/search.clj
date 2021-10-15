(ns aoc.search
  (:require
   [clojure.data.priority-map :refer [priority-map]]))

(defn dijkstra [start adjacent priority goal?]
  (loop [queue (priority-map start (priority start))
         seen #{start}]
    (let [state (first (peek queue))]
      (if (goal? state)
        state
        (let [as (remove seen (adjacent state))]
          (recur (reduce (fn [q a] (assoc q a (priority a))) (pop queue) as)
                 (into seen as)))))))
