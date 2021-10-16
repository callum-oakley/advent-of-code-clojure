(ns aoc.vectors)

(defn +v [& vs]
  (apply mapv + vs))

(defn *v [a v]
  (mapv #(* a %) v))
