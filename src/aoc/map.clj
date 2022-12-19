(ns aoc.map)

(defn +m [a b]
  (merge-with + a b))

(defn -m [a b]
  (merge-with - a b))
