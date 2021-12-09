(ns aoc.grid
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [aoc.vector :refer [+v]]))

(def north [-1 0])
(def east [0 1])
(def south [1 0])
(def west [0 -1])

(def right
  {north east east south south west west north})

(def left
  (set/map-invert right))

(defn parse [s]
  (first (reduce (fn [[g y] line]
                   [(first (reduce (fn [[g x] c]
                                     [(assoc g [y x] c) (inc x)])
                                   [g 0]
                                   line))
                    (inc y)])
                 [{} 0]
                 (str/split-lines s))))

(defn adjacent
  ([pos] (->> [north east south west] (map #(+v pos %))))
  ([pos g] (filter g (adjacent pos))))
