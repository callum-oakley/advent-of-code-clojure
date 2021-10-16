(ns aoc.search
  (:require
   [clojure.data.priority-map :refer [priority-map]])
  (:import
   [clojure.lang IPersistentCollection IPersistentStack PersistentQueue]))

(deftype PriorityQueue [priority-map priority]
  IPersistentCollection
  (cons [_ v] (PriorityQueue. (assoc priority-map v (priority v)) priority))
  IPersistentStack
  (peek [_] (first (peek priority-map)))
  (pop [_] (PriorityQueue. (pop priority-map) priority)))

(defn- generic [queue start adjacent goal?]
  (loop [queue (conj queue start)
         seen #{start}]
    (if (goal? (peek queue))
      (peek queue)
      (let [as (remove seen (adjacent (peek queue)))]
        (recur (into (pop queue) as) (into seen as))))))

(defn bfs [start adjacent goal?]
  (generic PersistentQueue/EMPTY start adjacent goal?))

(defn dfs [start adjacent goal?]
  (generic [] start adjacent goal?))

(defn dijkstra [priority start adjacent goal?]
  (generic (->PriorityQueue (priority-map) priority) start adjacent goal?))
