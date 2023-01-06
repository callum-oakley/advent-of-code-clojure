(ns aoc.2015.16
  (:require
   [clojure.string :as str]))

(defn parse [s]
  (map (fn [line]
         (let [[_ sue stats] (re-matches #"Sue (\d+): (.*)" line)]
           [(read-string sue) (->> (re-seq #"(\w+): (\d+)" stats)
                                   (map (fn [[_ k v]]
                                          [(keyword k) (read-string v)]))
                                   (into {}))]))
       (str/split-lines s)))

(defn part-* [target-preds aunts]
  (some (fn [[sue stats]]
          (when (every? (fn [[k p]]
                          (or (nil? (k stats))
                              (p (k stats))))
                        target-preds)
            sue))
        aunts))

(def target-vals
  {:children 3   :cats 7       :samoyeds 2   :pomeranians 3   :akitas 0
   :vizslas 0    :goldfish 5   :trees 3      :cars 2          :perfumes 1})

(defn part-1 [aunts]
  (part-* (map (fn [[k v]] [k #(= v %)]) target-vals) aunts))

(defn part-2 [aunts]
  (part-* (map (fn [[k v]]
                 [k (condp contains? k
                      #{:cats :trees} #(< v %)
                      #{:pomeranians :goldfish} #(> v %)
                      #(= v %))])
               target-vals)
          aunts))
