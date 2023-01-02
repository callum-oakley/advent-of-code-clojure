(ns aoc.2016.23
  (:require
   [clojure.string :as str]))

(defn parse [s]
  (mapv #(mapv read-string (re-seq #"\S+" %)) (str/split-lines s)))

(defn tgl [m i]
  (if (contains? m i)
    (update-in m [i 0] '{inc dec dec inc tgl inc jnz cpy cpy jnz})
    m))

(defn run [m head reg]
  (if (= 2 head) ;; instructions 2 to 9 multiply a by b
    (recur m 10 (update reg 'a * (reg 'b)))
    (if-let [[op x y] (get m head)]
      (let [read #(if (int? %) % (reg %))]
        (case op
          cpy (recur m (inc head) (assoc reg y (read x)))
          inc (recur m (inc head) (update reg x inc))
          dec (recur m (inc head) (update reg x dec))
          jnz (recur m (if (zero? (read x)) (inc head) (+ (read y) head)) reg)
          tgl (recur (tgl m (+ (read x) head)) (inc head) reg)))
      (reg 'a))))

(defn part-1 [m]
  (run m 0 '{a 7 b 0 c 0 d 0}))

(defn part-2 [m]
  (run m 0 '{a 12 b 0 c 0 d 0}))
