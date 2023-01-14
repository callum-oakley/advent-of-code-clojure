(ns aoc.2018.24 
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def group-re
  #"(\d+) units each with (\d+) hit points (\([^)]+\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)")

(defn parse-group [army s]
  (let [[_ units hp mods damage type initiative] (re-find group-re s)]
    {:army army
     :units (read-string units)
     :hit-points (read-string hp)
     :mods (reduce (fn [m mod]
                     (let [[effect _ & types]
                           (map read-string (re-seq #"\w+" mod))]
                       (assoc m effect (set types))))
                   {}
                   (when mods (str/split mods #";")))
     :damage (read-string damage)
     :type (read-string type)
     :initiative (read-string initiative)}))

(defn parse [s]
  (into {} (mapcat
            (fn [army s]
              (map-indexed
               (fn [i line]
                 [(symbol (str army "-" (inc i))) (parse-group army line)])
               (rest (str/split-lines s))))
            ['immune-system 'infection]
            (str/split s #"\n\n"))))

(defn effective-power [{:keys [units damage]}]
  (* units damage))

(defn damage [attacker defender]
  (let [immune ((:mods defender) 'immune) weak ((:mods defender) 'weak)]
    (cond
      (= (:army attacker) (:army defender)) 0
      (contains? immune (:type attacker)) 0
      (contains? weak (:type attacker)) (* 2 (effective-power attacker))
      :else (effective-power attacker))))

(defn select-targets [groups]
  (->> (keys groups)
       (sort-by (fn [id]
                  [(effective-power (groups id)) (:initiative (groups id))])
                #(compare %2 %1))
       (reduce
        (fn [[attacks defenders] attacker]
          (if-let [defender
                   (->> defenders
                        (filter #(pos? (damage (groups attacker) (groups %))))
                        (sort-by (fn [defender]
                                   [(damage (groups attacker) (groups defender))
                                    (effective-power (groups defender))
                                    (:initiative (groups defender))])
                                 #(compare %2 %1))
                        first)]
            [(conj attacks {:attacker attacker :defender defender})
             (disj defenders defender)]
            [attacks defenders]))
        [[] (set (keys groups))])
       first
       (sort-by #(:initiative (groups (:attacker %))) >)))

(defn apply-attack [groups {:keys [attacker defender]}]
  (if-let [a (groups attacker)]
    (let [d (groups defender)
          survivors (- (:units d) (quot (damage a d) (:hit-points d)))]
      (if (pos? survivors)
        (assoc-in groups [defender :units] survivors)
        (dissoc groups defender)))
    groups))

(defn fight [groups]
  (let [armies (distinct (map :army (vals groups)))]
    (if (= 1 (count armies))
      {:result (first armies)
       :units (apply + (map :units (vals groups)))}
      (let [groups* (reduce apply-attack groups (select-targets groups))]
        ;; We can get stuck in an infinite loop where neither army has the
        ;; damage necessary to kill any more of the other.
        (if (= groups groups*)
          {:result :stalemate}
          (recur groups*))))))

(defn part-1 [groups]
  (:units (fight groups)))

(defn part-2 [groups]
  (let [fight* (fn [boost]
                 (fight (update-vals groups #(if (= 'immune-system (:army %))
                                               (update % :damage + boost)
                                               %))))
        high (loop [boost 1]
               (if (= 'immune-system (:result (fight* boost)))
                 boost
                 (recur (* 2 boost))))]
    (loop [low (quot high 2) high high]
      (if (= 1 (- high low))
        (:units (fight* high))
        (let [mid (quot (+ low high) 2)]
          (if (= 'immune-system (:result (fight* mid)))
            (recur low mid)
            (recur mid high)))))))

(def example
  "Immune System:
   17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
   989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

   Infection:
   801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
   4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")

(deftest test-examples
  (is (= 5216 (part-1 (parse example))))
  (is (= 51 (part-2 (parse example)))))
