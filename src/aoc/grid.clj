(ns aoc.grid
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [aoc.vector :refer [+v]]))

(def north [-1 0])
(def north-east [-1 1])
(def east [0 1])
(def south-east [1 1])
(def south [1 0])
(def south-west [1 -1])
(def west [0 -1])
(def north-west [-1 -1])

(def right
  {north east east south south west west north})

(def left
  (set/map-invert right))

(defn parse
  ([s] (parse s identity))
  ([s parse-c]
   (first (reduce (fn [[g y] line]
                    [(first (reduce (fn [[g x] c]
                                      [(assoc g [y x] (parse-c c)) (inc x)])
                                    [g 0]
                                    line))
                     (inc y)])
                  [{} 0]
                  (str/split-lines s)))))

(defn adjacent
  ([pos] (->> [north east south west] (map #(+v pos %))))
  ([pos g] (filter g (adjacent pos))))

(defn adjacent-8
  ([pos]
   (->> [north north-east east south-east south south-west west north-west]
        (map #(+v pos %))))
  ([pos g] (filter g (adjacent-8 pos))))
