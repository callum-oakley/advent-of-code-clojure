(ns aoc.2017.24
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (reduce (fn [m component]
            (let [[a b] (->> component (re-seq #"\d+") (map read-string))]
              (-> m
                  (update a (fnil conj #{}) b)
                  (update b (fnil conj #{}) a))))
          {}
          (str/split-lines s)))

(defn bridges [bridge components]
  (let [from (peek bridge)]
    (if-let [tos (seq (components from))]
      (mapcat (fn [to]
                (bridges (conj bridge from to)
                         (-> components
                             (update from disj to)
                             (update to disj from))))
              tos)
      [bridge])))

(defn part-1 [components]
  (->> components (bridges [0]) (map #(apply + %)) (apply max)))

(defn part-2 [components]
  (let [bs (bridges [0] components)
        longest (->> bs (map count) (apply max))]
    (->> bs (filter #(= longest (count %))) (map #(apply + %)) (apply max))))

(deftest test-examples
  (is (= 31 (part-1 (parse "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"))))
  (is (= 19 (part-2 (parse "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10")))))
