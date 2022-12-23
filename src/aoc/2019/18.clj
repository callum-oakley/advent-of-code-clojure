(ns aoc.2019.18
  (:require
   [aoc.grid :as g]
   [aoc.search :as search]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

;; It's convenient to treat @ 0 1 2 3 as keys
(defn key? [tile]
  (or (#{\@ \0 \1 \2 \3} tile) (java.lang.Character/isLowerCase tile)))
(defn door? [tile] (java.lang.Character/isUpperCase tile))
(defn ->key [door] (java.lang.Character/toLowerCase door))

(defn parse [s]
  (into {} (remove (fn [[_ tile]] (= \# tile))) (g/parse s)))

;; Distinguish the four entrances by labelling them 0 1 2 3
(defn correct-grid [g]
  (let [e (some (fn [[pos tile]] (when (= \@ tile) pos)) g)]
    (apply assoc
           (apply dissoc g (g/adjacent-5 e))
           (interleave (g/adjacent-corners e) [\0 \1 \2 \3]))))

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
(defn part-* [robots g]
  (let [keys (->> g vals (filter key?) set)
        kg (key-graph g)]
    (:steps
     (search/dijkstra
      {:robots robots :steps 0 :keys (set robots)}
      (fn [state]
        (mapcat (fn [n]
                  (keep #(when (set/subset? (:keys %) (:keys state))
                           (-> state
                               (assoc-in [:robots n] (:pos %))
                               (update :steps + (:steps %))
                               (update :keys conj (:pos %))))
                        (kg (get-in state [:robots n]))))
                (range (count (:robots state)))))
      (juxt :robots :keys)
      #(= keys (:keys %))
      :steps))))

(defn part-1 [g]
  (part-* [\@] g))

(defn part-2 [g]
  (part-* [\0 \1 \2 \3] (correct-grid g)))

(def examples-1
  (map
   #(str/join "\n" %)
   [["b.A.@.a"]
    ["f.D.E.e.C.b.A.@.a.B.c." "#####################." "d....................."]
    ["...............b.C.D.f" ".#####################" ".....@.a.B.c.d.A.e.F.g"]
    ["i.G..c...e..H.p" "#######.#######" "j.A..b...f..D.o" "#######@#######"
     "k.E..a...g..B.n" "#######.#######" "l.F..d...h..C.m"]
    ["@..............ac.GI.b" "##d#e#f###############" "##A#B#C###############"
     "##g#h#i###############"]]))

(def examples-2
  (map
   #(str/join "\n" %)
   [["a.#Cd" "#0#1#" "#####" "#2#3#" "cB#Ab"]
    ["d.ABC.#.....a" "#####0#1#####" "#############" "#####2#3#####"
     "b.....#.....c"]
    ["DcBa.#.GhKl" ".###0#1#I##" "e#d#####j#k" "##C#2#3###J" "fEbA.#.FgHi"]
    ["g#f.D#..h#l" "F###e#E###." "dCba0#1BcIJ" "###########" "nK.L2#3G..."
     "M###N#H###." "o#m..#i#jk."]]))

(deftest test-examples
  (is (= [8 86 132 136 81] (map #(part-* [\@] (parse %)) examples-1)))
  (is (= [8 24 32 72] (map #(part-* [\0 \1 \2 \3] (parse %)) examples-2))))
