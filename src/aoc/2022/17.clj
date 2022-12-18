(ns aoc.2022.17 
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def rocks
  [[[0 0] [0 1] [0 2] [0 3]]
   [[0 1] [1 0] [1 1] [1 2] [2 1]]
   [[0 0] [0 1] [0 2] [1 2] [2 2]]
   [[0 0] [1 0] [2 0] [3 0]]
   [[0 0] [0 1] [1 0] [1 1]]])

(defn parse [s]
  (mapv #(case % \< grid/west \> grid/east) (str/trim s)))

(defn push [rock dir]
  (let [rock* (map #(+v % dir) rock)]
    (if (every? #(<= 0 (second %) 6) rock*) rock* rock)))

(defn simulate [jets]
  (iterate (fn [{:keys [top tower j r]}]
             (loop [j j rock (push (rocks (mod r 5)) [(+ top 4) 2])]
               (let [rock* (push rock (jets j))
                     rock (if (some tower rock*) rock rock*)
                     rock* (push rock grid/north)]
                 (if (some #(or (tower %) (zero? (first %))) rock*)
                   {:top (apply max top (map first rock))
                    :tower (into tower rock)
                    :j (mod (inc j) (count jets))
                    :r (inc r)}
                   (recur (mod (inc j) (count jets)) rock*)))))
           {:top 0 :tower #{} :j 0 :r 0}))

(defn part-1 [jets]
  (->> jets simulate (drop 2022) first :top))

;; After looking at states where r is a multiple of 5 and grouping by j it's
;; clear that the height settles in to a linear sequence. We can extrapolate
;; from the first pair where the period evenly divides the target.
(defn part-2 [jets]
  (loop [states (simulate jets) seen {}]
    (let [state (first states)]
      (if (zero? (mod (:r state) 5))
        (or (when-let [prev (seen (:j state))]
              (let [r (:r prev)
                    dr (- (:r state) r)
                    top (:top prev)
                    dtop (- (:top state) top)
                    cycles (/ (- 1000000000000 r) dr)]
                (when (int? cycles) (+ top (* dtop cycles)))))
            (recur (rest states) (assoc seen (:j state) state)))
        (recur (rest states) seen)))))

(def example
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(deftest test-example
  (is (= 3068 (part-1 (parse example))))
  (is (= 1514285714288 (part-2 (parse example)))))
