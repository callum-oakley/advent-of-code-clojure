(ns aoc.2019.18
  (:require
   [aoc.grid :as g]
   [aoc.search :as search]
   [clojure.math.combinatorics :as comb]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn wall? [tile] (= \# tile))
(defn floor? [tile] (= \. tile))
;; It's convenient to treat \@ as a key
(defn key? [tile] (or (= \@ tile) (java.lang.Character/isLowerCase tile)))
(defn door? [tile] (java.lang.Character/isUpperCase tile))
(defn ->key [door] (java.lang.Character/toLowerCase door))

(defn parse [s]
  (into {} (remove (fn [[_ tile]] (wall? tile))) (g/parse s)))

;; The distance of the shortest path from from a to b, and the keys required
;; along the way -- if such a path exists.
(defn path [g a b]
  (dissoc
   (search/bfs
    {:pos a :steps 0 :keys #{}}
    (fn [{:keys [pos steps keys]}]
      (->> (g/adjacent pos g)
           (filter #(or (= b %) (door? (g %)) (floor? (g %))))
           (map #(if (door? (g %))
                   {:pos % :steps (inc steps) :keys (conj keys (->key (g %)))}
                   {:pos % :steps (inc steps) :keys keys}))))
    #(= b (:pos %))
    :pos)
   :pos))

;; TODO we could make this faster by traversing the grid from each start point
;; once, rather than checking every pair.
(defn paths [g]
  (let [keys (keep (fn [[pos tile]] (when (key? tile) pos)) g)]
    (reduce (fn [ps [a b]]
              (if-let [p (path g a b)]
                (merge-with merge ps {(g a) {(g b) p} (g b) {(g a) p}})
                ps))
            {}
            (comb/combinations keys 2))))

(defn part-* [g]
  (let [keys (->> g vals (filter key?) set)
        ps (paths g)]
    (:steps
     (search/dijkstra
      :steps
      {:pos \@ :steps 0 :keys #{\@}}
      (fn [{:keys [pos steps keys]}]
        (keep (fn [[pos p]]
                (when (set/subset? (:keys p) keys)
                  {:pos pos :steps (+ steps (:steps p)) :keys (conj keys pos)}))
              (ps pos)))
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
