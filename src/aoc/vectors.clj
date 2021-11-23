(ns aoc.vectors)

(defn +v [& vs]
  (apply mapv + vs))

(defn *v [a v]
  (mapv #(* a %) v))

(defn transpose [vs]
  (apply mapv vector vs))

(defn manhattan-distance [pos]
  (apply + (map #(Math/abs %) pos)))
