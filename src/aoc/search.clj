(ns aoc.search
  (:require
   [clojure.data.priority-map :as pm]
   [clojure.test :refer [deftest is]])
  (:import
   [clojure.lang IPersistentCollection IPersistentStack PersistentQueue]))

(deftype PriorityQueue [priority-map priority]
  IPersistentCollection
  (cons [_ v] (PriorityQueue. (assoc priority-map v (priority v)) priority))
  IPersistentStack
  (peek [_] (first (peek priority-map)))
  (pop [_] (PriorityQueue. (pop priority-map) priority)))

(defn priority-queue-min-first [priority]
  (->PriorityQueue (pm/priority-map) priority))

(defn priority-queue-max-first [priority]
  (->PriorityQueue (pm/priority-map-by #(compare %2 %1)) priority))

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
  (let [queue (priority-queue-min-first cost)]
    (first (filter goal? (traversal queue start adjacent normalise)))))

(defn a* [start adjacent normalise goal? cost heuristic]
  (dijkstra start adjacent normalise goal? #(+ (cost %) (heuristic %))))

(defn branch-and-bound
  "Finds the highest score solution in the search space, exploring highest score
   branches first and using bound to eliminate branches that can't contain
   better solutions than we've already seen."
  [start adjacent normalise score bound]
  (let [<= (fn [a b] (<= (compare a b) 0))]
    (loop [queue (conj (priority-queue-max-first score) start)
           visited #{}
           best (score start)]
     (if-let [current (peek queue)]
       (if (or (<= (bound current) best) (visited (normalise current)))
         (recur (pop queue) visited best)
         (let [best (if (<= (score current) best) best (score current))]
           (recur (->> (adjacent current)
                       (remove #(or (<= (bound %) best)
                                    (visited (normalise %))))
                       (into (pop queue)))
                  (conj visited (normalise current))
                  best)))
       best))))

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
