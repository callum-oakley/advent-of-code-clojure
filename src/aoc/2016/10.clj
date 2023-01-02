(ns aoc.2016.10
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce
   (fn [state line]
     (if-let [[_ value bot] (re-matches #"value (\d+) goes to (bot \d+)" line)]
       (update-in state [bot :values] (fnil conj []) (read-string value))
       (let [[_ bot low high]
             (re-matches
              #"(bot \d+) gives low to (\w+ \d+) and high to (\w+ \d+)"
              line)]
         (update state bot (fnil assoc {:values []}) :low low :high high))))
   {}
   (map str/trim (str/split-lines s))))

(defn run [state]
  (if-let [ready (some (fn [[k v]] (when (= 2 (count (:values v))) k)) state)]
    (let [[low high] (sort (get-in state [ready :values]))]
      (-> state
          (assoc-in [ready :values] [])
          (update-in [ready :log] (fnil conj #{}) #{low high})
          (update-in [(get-in state [ready :low]) :values] (fnil conj []) low)
          (update-in [(get-in state [ready :high]) :values] (fnil conj []) high)
          recur))
    state))

(defn check-logs [state target]
  (->> state
       (some (fn [[k v]] (when (contains? (:log v) target) k)))
       (re-find #"\d+")
       read-string))

(defn check-outputs [state bins]
  (map #(first (get-in state [(format "output %d" %) :values])) bins))

(defn part-1 [state]
  (-> state run (check-logs #{17 61})))

(defn part-2 [state]
  (apply * (-> state run (check-outputs [0 1 2]))))

(deftest test-sample
  (let [state (run (parse "value 5 goes to bot 2
                           bot 2 gives low to bot 1 and high to bot 0
                           value 3 goes to bot 1
                           bot 1 gives low to output 1 and high to bot 0
                           bot 0 gives low to output 2 and high to output 0
                           value 2 goes to bot 2"))]
    (is (= [5 2 3] (check-outputs state [0 1 2])))
    (is (= 2 (check-logs state #{2 5})))))
