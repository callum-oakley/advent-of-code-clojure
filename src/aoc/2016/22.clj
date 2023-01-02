(ns aoc.2016.22
  (:require
   [aoc.search :as search]
   [aoc.vector :refer [+v]]
   [clojure.string :as str]))

(defn parse [s]
  (->> s str/split-lines (drop 2)
       (map #(let [[x y size used] (map read-string (re-seq #"\d+" %))]
               [[x y] {:size size :used used}]))
       (into {})))

(defn viable? [a b]
  (and (pos? (:used a)) (<= (+ (:used a) (:used b)) (:size b))))

(defn part-1 [state]
  (count (for [[pos-a a] state
               [pos-b b] state
               :when (and (not= pos-a pos-b) (viable? a b))] 1)))

;; Assumes that the only valid moves are moving data in to the unique empty
;; node, so we only need to try the four moves around the empty node.
(defn adjacent [{:keys [state hole goal steps]}]
  (for [node (map #(+v hole %) [[1 0] [-1 0] [0 1] [0 -1]])
        :when (and (contains? state node)
                   (viable? (state node) (state hole)))]
    {:state (-> state
                (assoc-in [node :used] 0)
                (assoc-in [hole :used] (:used (state node))))
     :hole node
     :goal (if (= node goal) hole goal)
     :steps (inc steps)}))

(defn part-2 [state]
  (:steps
   (search/bfs {:state state
                :hole (some (fn [[k v]] (when (zero? (:used v)) k)) state)
                :goal [29 0]
                :steps 0}
               adjacent
               (juxt :hole :goal)
               #(= [0 0] (:goal %)))))
