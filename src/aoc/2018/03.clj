(ns aoc.2018.03)

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string) (partition 5)))

(defn square [x0 y0 w h]
  (for [x (range x0 (+ x0 w)) y (range y0 (+ y0 h))] [x y]))

(defn fabric [claims]
  (reduce (fn [fabric [_ x0 y0 w h]]
            (reduce (fn [fabric [x y]] (update fabric [x y] (fnil inc 0)))
                    fabric
                    (square x0 y0 w h)))
          {}
          claims))

(defn part-1 [claims]
  (->> claims fabric vals (filter #(< 1 %)) count))

(defn part-2 [claims]
  (let [fabric (fabric claims)]
    (some (fn [[id x0 y0 w h]]
            (when (every? #(= 1 (fabric %)) (square x0 y0 w h)) id))
          claims)))
