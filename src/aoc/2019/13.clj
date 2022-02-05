(ns aoc.2019.13
  (:require
   [aoc.2019.intcode :as i]))

(defn play [& {:keys [slow? println]}]
  (loop [vm (i/io (i/run (assoc (i/load "input/2019/13") 0 2)) [])
         pad nil ball nil score nil]
    (if-let [[x y t] (seq (take 3 (:out vm)))]
      (let [vm (update vm :out #(drop 3 %))]
        (println (if (= [-1 0] [x y])
                   (format "\u009B%s;%sH%s\u0007" 1 2 t)
                   (format "\u009B%s;%sH%s" (+ 2 y) (+ 2 x)
                           (case t 0 \space 1 \# 2 \= 3 \- 4 \o))))
        (cond
          (= [-1 0] [x y]) (recur vm pad ball t)
          (= 3 t) (recur vm x ball score)
          (= 4 t) (recur vm pad x score)
          :else (recur vm pad ball score)))
      (if (= :halt (:state vm))
        score
        (do
          (when slow? (Thread/sleep 10))
          (recur (i/io vm [(cond (< pad ball) 1 (< ball pad) -1 :else 0)])
                 pad ball score))))))

(defn part-1 []
  (count (filter (fn [[x y t]] (= 2 t))
                 (partition 3 (i/run-io (i/load "input/2019/13") [])))))

(defn part-2 []
  (play :slow? false :println (constantly nil)))

(defn -main []
  (println "\u009B?25l\u009B2J")
  (play :slow? true :println println)
  (println (format "\u009B?25h\u009B%d;%dH" 28 0)))
