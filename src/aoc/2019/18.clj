(ns aoc.2019.18
  (:require
   [aoc.grid :as g]
   [aoc.search :as search]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; It's convenient to treat @ as a key
(defn key? [tile] (or (= \@ tile) (java.lang.Character/isLowerCase tile)))
(defn door? [tile] (java.lang.Character/isUpperCase tile))
(defn ->key [door] (java.lang.Character/toLowerCase door))

(defn parse [s]
  (into {} (remove (fn [[_ tile]] (= \# tile))) (g/parse s)))

;; All keys reachable from pos, along with the steps and keys required.
(defn reachable [g pos]
  (->> (search/bft
        {:pos pos :steps 0 :keys #{}}
        (fn [state]
          (when (or (= pos (:pos state)) (not (key? (g (:pos state)))))
            (map #(-> state
                      (assoc :pos %)
                      (update :steps inc)
                      (cond-> (door? (g %))
                        (update :keys conj (->key (g %)))))
                 (g/adjacent (:pos state) g))))
        :pos)
       rest
       (map #(update % :pos g))
       (filter #(key? (:pos %)))))

(defn key-graph [g]
  (reduce (fn [kg pos] (assoc kg (g pos) (reachable g pos)))
          {}
          (keep (fn [[pos tile]] (when (key? tile) pos)) g)))

;; Compute a weighted graph of the distance between keys up front, along with
;; the keys required to traverse each edge, then dijkstra on the higher level
;; key graph.
(defn part-* [g]
  (let [keys (->> g vals (filter key?) set)
        kg (key-graph g)]
    (:steps
     (search/dijkstra :steps
                      {:pos \@ :steps 0 :keys #{\@}}
                      (fn [state]
                        (keep #(when (set/subset? (:keys %) (:keys state))
                                 (-> state
                                     (assoc :pos (:pos %))
                                     (update :steps + (:steps %))
                                     (update :keys conj (:pos %))))
                              (kg (:pos state))))
                      #(= keys (:keys %))
                      (juxt :pos :keys)))))

(defn part-1 []
  (->> "input/2019/18" slurp parse part-*))

(def examples
  (map
   #(str/join "\n" %)
   [["b.A.@.a"]
    ["f.D.E.e.C.b.A.@.a.B.c." "#####################." "d....................."]
    ["...............b.C.D.f" ".#####################" ".....@.a.B.c.d.A.e.F.g"]
    ["i.G..c...e..H.p" "#######.#######" "j.A..b...f..D.o" "#######@#######"
     "k.E..a...g..B.n" "#######.#######" "l.F..d...h..C.m"]
    ["@..............ac.GI.b" "##d#e#f###############" "##A#B#C###############"
     "##g#h#i###############"]]))

(deftest test-examples
  (is (= [8 86 132 136 81] (map #(part-* (parse %)) examples))))
