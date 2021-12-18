(ns aoc.search
  (:require
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.test :refer [deftest is]])
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
   ((fn go [queue visited]
      (lazy-seq
       (when-let [current (peek queue)]
         (if (visited (normalise current))
           (go (pop queue) visited)
           (cons current
                 (go (->> (adjacent current)
                          (remove #(visited (normalise %)))
                          (into (pop queue)))
                     (conj visited (normalise current))))))))
    (conj queue start)
    #{})))

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

(deftest test-dijkstra-corner-case
  ;; This was wrong in my first implementation (I was only considering the cost
  ;; of a node the first time you encountered it) so, a regression test:
  (is (= 2 (:cost
            (dijkstra :cost
                      {:pos 'a :cost 0}
                      {{:pos 'a :cost 0} [{:pos 'b :cost 3} {:pos 'c :cost 1}]
                       {:pos 'c :cost 1} [{:pos 'b :cost 2}]}
                      #(= 'b (:pos %))
                      :pos)))))
