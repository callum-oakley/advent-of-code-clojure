(ns aoc.2015.21
  (:require
   [clojure.math.combinatorics :as comb]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map read-string (re-seq #"\d+" s)))

(defn items [& ns]
  (map (fn [[c d a]] {:cost c :damage d :armor a}) (partition 3 ns)))

(defn stats []
  (let [weapons (items 8 4 0 10 5 0 25 6 0 40 7 0 74 8 0)
        armor (items 13 0 1 31 0 2 53 0 3 75 0 4 102 0 5)
        rings (items 25 1 0 50 2 0 100 3 0 20 0 1 40 0 2 80 0 3)]
    (for [w weapons
          a (cons nil armor)
          rs (mapcat #(comb/combinations rings %) [0 1 2])]
      (apply merge-with + w a rs))))

(defn fight [attacker defender]
  (if (pos? (:hit-points attacker))
    (recur (update defender
                   :hit-points
                   - (max 1 (- (:damage attacker) (:armor defender))))
           attacker)
    defender))

(defn part-* [winner opt [hp d a]]
  (let [boss {:id :boss :hit-points hp :damage d :armor a}]
    (->> (stats)
         (filter #(= winner
                     (:id (fight (assoc % :id :player :hit-points 100) boss))))
         (map :cost)
         (apply opt))))

(defn part-1 [player]
  (part-* :player min player))

(defn part-2 [player]
  (part-* :boss max player))

(deftest test-fight
  (is (= {:id :player :hit-points 2 :damage 5 :armor 5}
         (fight {:id :player :hit-points 8 :damage 5 :armor 5}
                {:id :boss :hit-points 12 :damage 7 :armor 2}))))
