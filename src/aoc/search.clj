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

(defn- traversal [queue start adjacent normalise]
  ((fn go [queue visited]
     (lazy-seq
      (when-let [current (peek queue)]
        (if (visited (normalise current))
          (go (pop queue) visited)
          (cons current (go (->> (adjacent current)
                                 (remove #(visited (normalise %)))
                                 (into (pop queue)))
                            (conj visited (normalise current))))))))
   (conj queue start)
   #{}))

(defn bft [start adjacent normalise]
  (traversal PersistentQueue/EMPTY start adjacent normalise))

(defn dft [start adjacent normalise]
  (traversal [] start adjacent normalise))

(defn bfs [start adjacent normalise goal?]
  (first (filter goal? (bft start adjacent normalise))))

(defn dfs [start adjacent normalise goal?]
  (first (filter goal? (dft start adjacent normalise))))

(defn dijkstra [start adjacent normalise goal? cost]
  (let [queue (->PriorityQueue (priority-map) cost)]
    (first (filter goal? (traversal queue start adjacent normalise)))))

(defn a* [start adjacent normalise goal? cost heuristic]
  (dijkstra start adjacent normalise goal? #(+ (cost %) (heuristic %))))

;; This was wrong in my first implementation (I was only considering the cost of
;; a node the first time you encountered it) so, a regression test:
(deftest test-dijkstra-corner-case
  (is (= 2 (:cost
            (dijkstra {:pos 'a :cost 0}
                      {{:pos 'a :cost 0} [{:pos 'b :cost 3} {:pos 'c :cost 1}]
                       {:pos 'c :cost 1} [{:pos 'b :cost 2}]}
                      :pos
                      #(= 'b (:pos %))
                      :cost)))))
