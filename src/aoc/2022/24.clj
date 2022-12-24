(ns aoc.2022.24
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v]]
   [aoc.search :as search]
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [g (grid/parse s)
        [[_ h] [_ w]] (grid/box (keys g))]
    [(dec h) (dec w)
     (reduce (fn [blizzards [[y x] c]]
               (case c
                 \# blizzards
                 \. blizzards
                 (update blizzards [(dec y) (dec x)]
                         conj (case c
                                \^ grid/north \> grid/east
                                \v grid/south \< grid/west))))
             {}
             g)]))

(defn tick [h w blizzards]
  (reduce (fn [blizzards [pos dirs]]
            (reduce (fn [blizzards dir]
                      (let [[y x] (+v pos dir)]
                        (update blizzards [(mod y h) (mod x w)] conj dir)))
                    blizzards
                    dirs))
          {}
          blizzards))

(defn part-* [legs [h w blizzards]]
  (let [period (lcm h w)
        blizzards (vec (take period (iterate #(tick h w %) blizzards)))]
    (:mins
     (search/bfs
      {:pos [-1 0] :mins 0 :leg 0}
      (fn [{:keys [pos mins leg]}]
        (keep (fn [[y x]]
                (when (and (or (and (<= 0 y (dec h)) (<= 0 x (dec w)))
                               (#{[-1 0] [h (dec w)]} [y x]))
                           (not ((blizzards (mod (inc mins) period)) [y x])))
                  {:pos [y x]
                   :mins (inc mins)
                   :leg (cond (and (= 0 leg) (= [h (dec w)] [y x])) 1
                              (and (= 1 leg) (= [-1 0] [y x])) 2
                              :else leg)}))
              (grid/adjacent-5 pos)))
      identity
      #(and (= legs (:leg %)) (= [h (dec w)] (:pos %)))))))

(defn part-1 [input]
  (part-* 1 input))

(defn part-2 [input]
  (part-* 2 input))

(def example
  "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#")

(deftest test-example
  (is (= 18 (part-1 (parse example))))
  (is (= 54 (part-2 (parse example)))))
