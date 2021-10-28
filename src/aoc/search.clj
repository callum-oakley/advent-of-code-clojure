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

(defn- traversal
  ([queue start adjacent]
   (traversal queue start adjacent identity))
  ([queue start adjacent normalise]
   ((fn go [queue seen]
      (when (peek queue)
        (cons (peek queue)
              (lazy-seq
               (let [as (remove #(seen (normalise %)) (adjacent (peek queue)))]
                 (go (into (pop queue) as) (into seen (map normalise) as)))))))
    (conj queue start)
    #{(normalise start)})))

(defn bft [& opts]
  (apply traversal PersistentQueue/EMPTY opts))

(defn dft [& opts]
  (apply traversal [] opts))

(defn- search [queue start adjacent goal? & more]
   (first (filter goal? (apply traversal queue start adjacent more))))

(defn bfs [& opts]
  (apply search PersistentQueue/EMPTY opts))

(defn dfs [& opts]
  (apply search [] opts))

(defn dijkstra [cost & opts]
  (apply search (->PriorityQueue (priority-map) cost) opts))

(defn a* [cost heuristic & opts]
  (apply dijkstra #(+ (cost %) (heuristic %)) opts))
