(ns aoc.2020.21
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse-food [food]
  (let [[_ ingredients allergens] (re-matches #"(.+) \(contains (.+)\)" food)]
    [(set (str/split ingredients #" ")) (str/split allergens #", ")]))

(defn parse [s]
  (map parse-food (str/split-lines s)))

(defn identify-potentially-dangerous [foods]
  (->> foods
       (mapcat (fn [[ingredients allergens]]
                 (map (fn [allergen] {allergen ingredients}) allergens)))
       (apply merge-with set/intersection)))

(defn part-1 [foods]
  (->> (mapcat first foods)
       (remove (apply set/union (vals (identify-potentially-dangerous foods))))
       count))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

;; Adapted from day 16
(defn solve [potentially-dangerous dangerous]
  (let [dangerous? (fn [[_ ingredients]] (= (count ingredients) 1))]
    (if-let [[allergen ingredients]
             (first (filter dangerous? potentially-dangerous))]
      (recur
       (map-vals
        #(disj % (first ingredients))
        (dissoc potentially-dangerous allergen))
       (assoc dangerous allergen (first ingredients)))
      dangerous)))

(defn part-2 [foods]
  (->> (solve (identify-potentially-dangerous foods) {})
       (sort-by first)
       (map second)
       (str/join ",")))

(def sample
  (str "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n"
       "trh fvjkl sbzzf mxmxvkd (contains dairy)\n"
       "sqjhc fvjkl (contains soy)\n"
       "sqjhc mxmxvkd sbzzf (contains fish)"))

(deftest test-examples
  (is (= (part-1 (parse sample)) 5))
  (is (= (part-2 (parse sample)) "mxmxvkd,sqjhc,fvjkl")))
