;; Can delete this namespace when 1.11 is released
;; https://clojure.org/releases/devchangelog#_release_1_11_0

(ns aoc.map-updates)

(defn update-keys [m f]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn update-vals [m f]
  (into {} (map (fn [[k v]] [k (f v)]) m)))
