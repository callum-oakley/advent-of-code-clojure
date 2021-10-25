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

(defn- generic
  ([queue start adjacent goal?]
   (generic queue start adjacent goal? identity))
  ([queue start adjacent goal? normalise]
   (loop [queue (conj queue start)
          seen #{(normalise start)}]
     (if (goal? (peek queue))
       (peek queue)
       (let [as (remove #(seen (normalise %)) (adjacent (peek queue)))]
         (recur (into (pop queue) as) (into seen (map normalise as))))))))

(defn bfs [& opts]
  (apply generic PersistentQueue/EMPTY opts))

(defn dfs [& opts]
  (apply generic [] opts))

(defn dijkstra [cost & opts]
  (apply generic (->PriorityQueue (priority-map) cost) opts))

(defn a* [cost heuristic & opts]
  (apply dijkstra #(+ (cost %) (heuristic %)) opts))
