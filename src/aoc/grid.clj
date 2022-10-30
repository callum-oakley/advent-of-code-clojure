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
                                      [(if-let [pc (parse-c c)]
                                         (assoc g [y x] pc)
                                         g)
                                       (inc x)])
                                    [g 0]
                                    line))
                     (inc y)])
                  [{} 0]
                  (str/split-lines s)))))

(defn adjacent
  ([pos] (->> [north east south west] (map #(+v pos %))))
  ([pos g] (filter g (adjacent pos))))

(defn adjacent-5
  ([pos] (cons pos (adjacent pos)))
  ([pos g] (filter g (adjacent-5 pos))))

(defn adjacent-corners
  ([pos] (->> [north-east south-east south-west north-west] (map #(+v pos %))))
  ([pos g] (filter g (adjacent-corners pos))))

(defn adjacent-8
  ([pos] (interleave (adjacent pos) (adjacent-corners pos)))
  ([pos g] (filter g (adjacent-8 pos))))

(defn adjacent-9
  ([pos] (cons pos (adjacent-8 pos)))
  ([pos g] (filter g (adjacent-9 pos))))
