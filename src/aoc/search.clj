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

(defn priority-queue [priority]
  (->PriorityQueue (priority-map) priority))

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
  (let [queue (priority-queue cost)]
    (first (filter goal? (traversal queue start adjacent normalise)))))

(defn a* [start adjacent normalise goal? cost heuristic]
  (dijkstra start adjacent normalise goal? #(+ (cost %) (heuristic %))))

(defn branch-and-bound
  "Finds the lowest cost solution in the search space, exploring lowest cost
   branches first and using bound to eliminate branches that can't contain
   better solutions than we've already seen."
  [start adjacent normalise cost bound]
  (loop [queue (conj (priority-queue cost) start) visited #{} best ##Inf]
    (if-let [current (peek queue)]
      (if (or (<= best (bound current)) (visited (normalise current)))
        (recur (pop queue) visited best)
        (let [best (min best (cost current))]
          (recur (->> (adjacent current)
                      (remove #(or (<= best (bound %)) (visited (normalise %))))
                      (into (pop queue)))
                 (conj visited (normalise current))
                 best)))
      best)))

(defn branch-and-bound-max
  "Finds the highest score solution in the search space, exploring highest score
   branches first and using bound to eliminate branches that can't contain
   better solutions than we've already seen."
  [start adjacent normalise score bound]
  (- (branch-and-bound start adjacent normalise #(- (score %)) #(- (bound %)))))

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
