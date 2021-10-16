(ns aoc.2015.22
  (:require
   [aoc.map-updates :refer [update-vals]]
   [aoc.search :as search]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn apply-effects [state]
  (cond-> state
    (-> state :effects :shield) (assoc-in [:player :armor] 7)
    (-> state :effects :shield not) (assoc-in [:player :armor] 0)
    (-> state :effects :poison) (update-in [:boss :hit-points] - 3)
    (-> state :effects :recharge) (update-in [:player :mana] + 101)
    :always (update :effects #(->> (update-vals % dec)
                                   (filter (fn [[_ timer]] (pos? timer)))
                                   (into {})))))

(defn cast-attack [state mana damage healing]
  (-> state
      (update-in [:player :mana] - mana)
      (update :spent-mana + mana)
      (update-in [:boss :hit-points] - damage)
      (update-in [:player :hit-points] + healing)))

(defn cast-effect [state mana effect turns]
  (when-not (-> state :effects effect)
    (-> state
        (update-in [:player :mana] - mana)
        (update :spent-mana + mana)
        (update :effects assoc effect turns))))

(defn boss-attack [state]
  (update-in state
             [:player :hit-points]
             - (max 1 (- (-> state :boss :damage) (-> state :player :armor)))))

(defn alive? [state]
  (and (-> state :player :hit-points pos?) (-> state :player :mana neg? not)))

(defn next-phase [state]
  (update state :phase {:player-effect :player-turn
                        :player-turn :boss-effect
                        :boss-effect :boss-turn
                        :boss-turn :player-effect}))

(defn adjacent [difficulty state]
  (->> (case (:phase state)
         :player-effect [(apply-effects
                          (case difficulty
                            :normal state
                            :hard (update-in state [:player :hit-points] dec)))]
         :player-turn [(cast-attack state 53 4 0)
                       (cast-attack state 73 2 2)
                       (cast-effect state 113 :shield 6)
                       (cast-effect state 173 :poison 6)
                       (cast-effect state 229 :recharge 5)]
         :boss-effect [(apply-effects state)]
         :boss-turn [(boss-attack state)])
       (remove nil?)
       (filter alive?)
       (map next-phase)))

(defn part-* [difficulty]
  (let [[hp d] (map #(read-string (re-find #"\d+" %))
                    (str/split-lines (slurp "input/2015/22")))]
    (:spent-mana (search/dijkstra :spent-mana
                                  {:player {:hit-points 50 :mana 500 :armor 0}
                                   :boss {:hit-points hp :damage d}
                                   :effects {}
                                   :phase :player-effect
                                   :spent-mana 0}
                                  #(adjacent difficulty %)
                                  #(-> % :boss :hit-points pos? not)))))

(defn part-1 []
  (part-* :normal))

(defn part-2 []
  (part-* :hard))

(deftest test-answers
  (is (= 900 (part-1)))
  (is (= 1216 (part-2))))
