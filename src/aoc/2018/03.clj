(ns aoc.2018.03)

(defn square [x0 y0 w h]
  (for [x (range x0 (+ x0 w)) y (range y0 (+ y0 h))] [x y]))

(defn claims []
  (->> "input/2018/03" slurp (re-seq #"\d+") (map read-string) (partition 5)))

(defn fabric [claims]
  (reduce (fn [fabric [id x0 y0 w h]]
            (reduce (fn [fabric [x y]] (update fabric [x y] (fnil inc 0)))
                    fabric
                    (square x0 y0 w h)))
          {}
          claims))

(defn part-1 []
  (->> (claims) fabric vals (filter #(< 1 %)) count))

(defn part-2 []
  (let [claims (claims)
        fabric (fabric claims)]
    (some (fn [[id x0 y0 w h]]
            (when (every? #(= 1 (fabric %)) (square x0 y0 w h)) id))
          claims)))
