(ns aoc.2020.13
  (:require
   [aoc.number-theory :as number-theory]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[earliest-departure busses] (str/split-lines s)]
    [(read-string earliest-departure)
     (->> (str/split busses #",")
          (map-indexed (fn [i bus] {:index i :id (read-string bus)}))
          (filter (comp int? :id)))]))

(defn part-1 [[earliest-departure busses]]
  (let [wait #(mod (- earliest-departure) %)
        bus (apply min-key wait (map :id busses))]
    (* bus (wait bus))))

(defn part-2 [[_ busses]]
  (number-theory/chinese-remainder-theorem
   (map (fn [bus] [(mod (- (:index bus)) (:id bus)) (:id bus)]) busses)))

(def sample
  "939\n7,13,x,x,59,x,31,19")

(deftest test-examples
  (is (= (part-1 (parse sample)) 295))
  (is (= (part-2 (parse sample)) 1068781)))
