(ns aoc.2017.20
  (:require
   [aoc.vector :refer [+v manhattan-distance]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s str/split-lines
       (map-indexed
        (fn [i line]
          [i (->> line (re-seq #"-?\d+") (map read-string) (partition 3))]))
       (into {})))

(defn tick [[p v a]]
  [(+v p v a) (+v v a) a])

(defn collide [system]
  (first (reduce (fn [[system seen] [i [p _ _]]]
                   [(if (seen p) (dissoc system i (seen p)) system)
                    (assoc seen p i)])
                 [system {}]
                 system)))

(defn part-1 [system]
  (->> (nth (iterate #(update-vals % tick) system) 300)
       (apply min-key (fn [[_ [p _ _]]] (manhattan-distance p)))
       key))

(defn part-2 [system]
  (count (nth (iterate #(collide (update-vals % tick)) system) 100)))

(deftest test-example
  (is (= 0 (part-1 (parse "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
                           p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"))))
  (is (= 1 (part-2 (parse "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
                           p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
                           p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
                           p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>")))))
