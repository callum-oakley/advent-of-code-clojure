(ns aoc.2015.21
  (:require
   [aoc.map-updates :refer [update-vals]]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

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

(defn part-* [winner opt]
  (let [[hp d a] (map #(read-string (re-find #"\d+" %))
                      (str/split-lines (slurp "input/2015/21")))
        boss {:id :boss :hit-points hp :damage d :armor a}]
    (->> (stats)
         (filter #(= winner
                     (:id (fight (assoc % :id :player :hit-points 100) boss))))
         (map :cost)
         (apply opt))))

(defn part-1 []
  (part-* :player min))

(defn part-2 []
  (part-* :boss max))

(deftest test-fight
  (is (= {:id :player :hit-points 2 :damage 5 :armor 5}
         (fight {:id :player :hit-points 8 :damage 5 :armor 5}
                {:id :boss :hit-points 12 :damage 7 :armor 2}))))

(deftest test-answers
  (is (= 121 (part-1)))
  (is (= 201 (part-2))))
