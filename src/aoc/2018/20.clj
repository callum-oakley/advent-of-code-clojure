(ns aoc.2018.20
  (:require
   [aoc.grid :as grid]
   [aoc.search :as search]
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce
   (fn [[g active forks alts] c]
     (case c
       \^ [g active forks alts]
       \( [g active (conj forks active) (conj alts #{})]
       \| [g (peek forks) forks (conj (pop alts) (into (peek alts) active))]
       \) [g (into active (peek alts)) (pop forks) (pop alts)]
       \$ g
       (let [dir ({\N grid/north \E grid/east \S grid/south \W grid/west} c)]
         [(reduce (fn [g pos]
                    (-> g
                        (update pos (fnil conj #{}) (+v pos dir))
                        (update (+v pos dir) (fnil conj #{}) pos)))
                  g
                  active)
          (set (map #(+v % dir) active))
          forks
          alts])))
   [{} #{[0 0]} [] []]
   s))

(defn bft [g]
  (search/bft {:pos [0 0] :dist 0}
              #(map (fn [pos] {:pos pos :dist (inc (:dist %))}) (g (:pos %)))
              :pos))

(defn part-1 [g]
  (:dist (last (bft g))))

(defn part-2 [g]
  (->> g bft (filter #(<= 1000 (:dist %))) count))

(deftest test-examples
  (->> "^WNE$" parse part-1 (= 3) is)
  (->> "^ENWWW(NEEE|SSE(EE|N))$" parse part-1 (= 10) is)
  (->> "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" parse part-1 (= 18) is)
  (->> "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
       parse part-1 (= 23) is)
  (->> "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
       parse part-1 (= 31) is))
