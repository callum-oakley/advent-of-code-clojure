(ns aoc.vectors)

(defn +v [& vs]
  (apply mapv + vs))

(defn -v [& vs]
  (apply mapv - vs))

(defn *v [a v]
  (mapv #(* a %) v))

(defn div-v [v a]
  (mapv #(/ % a) v))

(defn transpose [vs]
  (apply mapv vector vs))

(defn manhattan-distance
  ([u v] (manhattan-distance (-v v u)))
  ([v] (apply + (map #(Math/abs %) v))))

(defn chessboard-distance
  ([u v] (chessboard-distance (-v v u)))
  ([v] (apply max (map #(Math/abs %) v))))
