(ns aoc.2019.13
  (:require
   [aoc.2019.intcode :as i]))

(def parse i/parse)

(defn play [mem & {:keys [interactive?]}]
  (loop [[out vm] (i/io (i/run (assoc mem 0 2)) [] 3)
         pad nil ball nil score nil]
    (if-let [[x y t] (seq out)]
      (do
        (when interactive?
          (println (if (= [-1 0] [x y])
                     (format "\u009B%s;%sH%s\u0007" 1 2 t)
                     (format "\u009B%s;%sH%s" (+ 2 y) (+ 2 x)
                             (case t 0 \space 1 \# 2 \= 3 \- 4 \o))))
          (Thread/sleep 5))
        (cond
          (= [-1 0] [x y]) (recur (i/io vm [] 3) pad ball t)
          (= 3 t) (recur (i/io vm [] 3) x ball score)
          (= 4 t) (recur (i/io vm [] 3) pad x score)
          :else (recur (i/io vm [] 3) pad ball score)))
      (case (:state vm)
        :in (recur (i/io vm [(cond (< pad ball) 1 (< ball pad) -1 :else 0)] 3)
                   pad ball score)
        :halt score))))

(defn part-1 [mem]
  (count (filter (fn [[_ _ t]] (= 2 t))
                 (partition 3 (i/run-io mem [])))))

(defn part-2 [mem]
  (play mem :interactive? false))

(defn -main []
  (println "\u009B?25l\u009B2J")
  (play (parse (slurp "input/2019/13")) :interactive? true)
  (println (format "\u009B?25h\u009B%d;%dH" 28 0)))
