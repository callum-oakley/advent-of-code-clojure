(ns aoc.2018.17
  (:require
   [aoc.vector :refer [+v -v]]
   [clojure.test :refer [deftest is]])
  (:import clojure.lang.PersistentQueue))

(defn parse [s]
  (->> (re-seq #"(x|y)=(\d+), (x|y)=(\d+)\.\.(\d+)" s)
       (map #(map read-string (rest %)))
       (reduce (fn [clay [axis-a a axis-b b0 b1]]
                 (case [axis-b axis-a]
                   [y x] (into clay (map (fn [y] [y a]) (range b0 (inc b1))))
                   [x y] (into clay (map (fn [x] [a x]) (range b0 (inc b1))))))
               #{})))

(defn flow [clay flowing settled block]
  (remove #(or (clay %) (flowing %) (settled %))
          [(-v block [0 1]) (+v block [0 1])]))

(defn settle [clay flowing settled block]
  (let [settle* (fn [dir]
                  (loop [block (+v block dir) settled* []]
                    (let [down (+v block [1 0])]
                      (cond
                        (clay block)
                        settled*

                        (and (flowing block) (or (clay down) (settled down)))
                        (recur (+v block dir) (conj settled* block))))))
        left (settle* [0 -1])
        right (settle* [0 1])]
    (when (and (vector? left) (vector? right))
      (concat left [block] right))))

(defn part-* [clay]
  (let [min-y (apply min (map first clay))
        max-y (apply max (map first clay))]
    (loop [flowing #{[0 500]}
           settled #{}
           queue (conj PersistentQueue/EMPTY [0 500])]
      (if-let [block (peek queue)]
        (let [down (+v block [1 0])]
          (cond
            (or (flowing down) (< max-y (first down)))
            (recur flowing settled (pop queue))

            (or (clay down) (settled down))
            (if-let [flowing* (seq (flow clay flowing settled block))]
              (recur (into flowing flowing*)
                     settled
                     (into (pop queue) flowing*))
              (let [settled* (settle clay flowing settled block)]
                (recur (apply disj flowing settled*)
                       (into settled settled*)
                       (into (pop queue)
                             (filter flowing (map #(-v % [1 0]) settled*))))))

            :else
            (recur (conj flowing down) settled (conj (pop queue) down))))
        (update-vals
         {:flowing flowing :settled settled}
         (fn [v] (count (filter #(<= min-y (first %) max-y) v))))))))

(defn part-1* [clay]
  (let [res (part-* clay)] (+ (:flowing res) (:settled res))))

(defn part-2* [clay]
  (:settled (part-* clay)))

(defn part-1 []
  (-> "input/2018/17" slurp parse part-1*))

(defn part-2 []
  (-> "input/2018/17" slurp parse part-2*))

(def example
  "x=495, y=2..7
   y=7, x=495..501
   x=501, y=3..7
   x=498, y=2..4
   x=506, y=1..2
   x=498, y=10..13
   x=504, y=10..13
   y=13, x=498..504")

(deftest test-example []
  (is (= 57 (part-1* (parse example))))
  (is (= 29 (part-2* (parse example)))))
