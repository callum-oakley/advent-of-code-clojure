(ns aoc.2016.25
  (:require
   [clojure.string :as str]))

(defn parse [s]
  (mapv #(mapv read-string (re-seq #"\S+" %)) (str/split-lines s)))

(defn run [m head reg]
  (when-let [[op x y] (get m head)]
    (let [read #(if (int? %) % (reg %))]
      (case op
        cpy (recur m (inc head) (assoc reg y (read x)))
        inc (recur m (inc head) (update reg x inc))
        dec (recur m (inc head) (update reg x dec))
        jnz (recur m (if (zero? (read x)) (inc head) (+ (read y) head)) reg)
        out (cons (read x) (lazy-seq (run m (inc head) reg)))))))

(defn part-1 [m]
  (let [signal (apply concat (repeat 50 [0 1]))]
    (some #(when (= signal
                    (take (count signal) (run m 0 {'a % 'b 0 'c 0 'd 0})))
             %)
          (range))))
