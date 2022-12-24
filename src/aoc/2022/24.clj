(ns aoc.2022.24
  (:require
   [aoc.grid :as grid]
   [aoc.vector :refer [+v manhattan-distance]]
   [aoc.search :as search]
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [g (grid/parse s)
        [[_ h] [_ w]] (grid/box (keys g))]
    [(dec h) (dec w)
     (reduce (fn [wind [[y x] c]]
               (case c
                 \# wind
                 \. wind
                 (update wind [(dec y) (dec x)]
                         conj (case c
                                \^ grid/north \> grid/east
                                \v grid/south \< grid/west))))
             {}
             g)]))

(defn tick [h w wind]
  (reduce (fn [wind [pos dirs]]
            (reduce (fn [wind dir]
                      (let [[y x] (+v pos dir)]
                        (update wind [(mod y h) (mod x w)] conj dir)))
                    wind
                    dirs))
          {}
          wind))

;; Each blizzard has a period of h or w, so the state has a period of (lcm h w)
(defn winds [h w wind]
  (vec (take (lcm h w) (iterate #(tick h w %) wind))))

(defn part-* [h w winds mins start goal]
  (:mins (search/a*
          {:pos start :mins mins :leg 0}
          (fn [{:keys [pos mins]}]
            (keep (fn [[y x]]
                    (when (and (or (and (<= 0 y (dec h)) (<= 0 x (dec w)))
                                   (#{start goal} [y x]))
                               (not ((winds (mod (inc mins)
                                                      (count winds)))
                                     [y x])))
                      {:pos [y x] :mins (inc mins)}))
                  (grid/adjacent-5 pos)))
          identity
          #(= goal (:pos %))
          :mins
          #(manhattan-distance (:pos %) goal))))

(defn part-1 [[h w wind]]
  (part-* h w (winds h w wind) 0 [-1 0] [h (dec w)]))

;; We can break the search in to three separate trips because it's always
;; possible to wait for favourable wind conditions so there's never any
;; advantage to doing a leg more slowly than possible.
(defn part-2 [[h w wind]]
  (let [winds (winds h w wind)
        mins (part-* h w winds 0 [-1 0] [h (dec w)])
        mins (part-* h w winds mins [h (dec w)] [-1 0])]
    (part-* h w winds mins [-1 0] [h (dec w)])))

(def example
  "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#")

(deftest test-example
  (is (= 18 (part-1 (parse example))))
  (is (= 54 (part-2 (parse example)))))
