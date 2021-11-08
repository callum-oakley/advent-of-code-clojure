(ns aoc.2020.13
  (:require
   [aoc.number-theory :as number-theory]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (str/split-lines (slurp "input/2020/13")))

(defn parse [[earliest-departure busses]]
  [(read-string earliest-departure)
   (->> (str/split busses #",")
        (map-indexed (fn [i bus] {:index i :id (read-string bus)}))
        (filter (comp int? :id)))])

(defn part-1
  ([] (part-1 (parse data)))
  ([[earliest-departure busses]]
   (let [wait #(mod (- earliest-departure) %)
         bus (apply min-key wait (map :id busses))]
     (* bus (wait bus)))))

(defn part-2
  ([] (part-2 (parse data)))
  ([[_ busses]]
   (number-theory/chinese-remainder-theorem
    (map (fn [bus] [(mod (- (:index bus)) (:id bus)) (:id bus)]) busses))))

(def sample
  ["939" "7,13,x,x,59,x,31,19"])

(deftest test-part-1
  (is (= (part-1 (parse sample)) 295))
  (is (= (part-1) 5257)))

(deftest test-part-2
  (is (= (part-2 (parse sample)) 1068781))
  (is (= (part-2) 538703333547789)))
