(ns aoc.string)

(defn remove-at [s i]
  (str (subs s 0 i) (subs s (inc i))))

(defn insert-at [s i c]
  (str (subs s 0 i) c (subs s i)))

(defn assoc-at [s i c]
  (str (subs s 0 i) c (subs s (inc i))))

(defn swap [s i j]
  (-> s (assoc-at i (get s j)) (assoc-at j (get s i))))

(defn rotate-left [s n]
  (str (subs s (mod n (count s))) (subs s 0 (mod n (count s)))))

(defn rotate-right [s n]
  (rotate-left s (- n)))
