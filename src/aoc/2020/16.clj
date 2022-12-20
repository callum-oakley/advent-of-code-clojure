(ns aoc.2020.16
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse-rule [rule]
  (let [[_ field & nums] (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" rule)
        [a b c d] (map read-string nums)]
    [field #(or (<= a % b) (<= c % d))]))

(defn parse-ticket [ticket]
  (mapv read-string (str/split ticket #",")))

(defn parse [s]
  (let [[rules your-ticket nearby-tickets]
        (map str/split-lines (str/split s #"\n\n"))]
    [(into {} (map parse-rule rules))
     (parse-ticket (second your-ticket))
     (map parse-ticket (rest nearby-tickets))]))

(defn valid? [rules ticket]
  (every? (apply some-fn (vals rules)) ticket))

(defn part-1 [[rules _ nearby-tickets]]
  (apply + (filter #(not (valid? rules [%])) (flatten nearby-tickets))))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

;; A map from field name to the set of valid columns for that field.
(defn build-free [rules columns]
  (map-vals
   (fn [rule]
     (set (filter #(every? rule (nth columns %)) (range (count rules)))))
   rules))

;; When a field in free only has one valid column choice we can move it to
;; fixed, and remove that column from every remaining field in free. Doing this
;; repeatedly fully determines the mapping from field to column.
(defn solve [free fixed]
  (let [fixed? (fn [[_ columns]] (= (count columns) 1))]
    (if-let [[field columns] (first (filter fixed? free))]
      (recur
       (map-vals #(disj % (first columns)) (dissoc free field))
       (assoc fixed field (first columns)))
      fixed)))

(defn part-2* [[rules _ nearby-tickets]]
  (let [valid (filter #(valid? rules %) nearby-tickets)
        columns (map (fn [i] (map #(nth % i) valid)) (range (count rules)))]
    (solve (build-free rules columns) {})))

(defn part-2 [[_ your-ticket _ :as args]]
  (let [solved (part-2* args)]
    (->> ["location" "station" "platform" "track" "date" "time"]
         (map #(nth your-ticket (get solved (str "departure " %))))
         (apply *))))

(def sample-1
  (str "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\n"
       "your ticket:\n7,1,14\n\n"
       "nearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"))

(def sample-2
  (str "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\n"
       "your ticket:\n11,12,13\n\n"
       "nearby tickets:\n3,9,18\n15,1,5\n5,14,9"))

(deftest test-examples
  (is (= (part-1 (parse sample-1)) 71))
  (is (= (part-2* (parse sample-2)) {"row" 0 "class" 1 "seat" 2})))
