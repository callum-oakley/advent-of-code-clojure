(ns aoc.2019.intcode)

(defn parse [s]
  (->> s (re-seq #"\d+") (mapv parse-long)))

(defn load [f]
  (parse (slurp f)))

(defn run [mem]
  (loop [mem mem head 0]
    (let [arg #(mem (+ % head))
          arg* #(mem (arg %))]
      (case (arg 0)
        1 (recur (assoc mem (arg 3) (+ (arg* 1) (arg* 2))) (+ head 4))
        2 (recur (assoc mem (arg 3) (* (arg* 1) (arg* 2))) (+ head 4))
        99 mem))))
