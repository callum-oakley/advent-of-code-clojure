(ns aoc.2021.15
  (:require
   [aoc.grid :as grid]
   [aoc.search :as search]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (grid/parse s #(- (int %) (int \0))))

(defn tile [cavern]
  (let [[height width] (->> cavern keys sort last (map inc))
        wrap-1-9 #(inc (mod (dec %) 9))]
    (into {} (for [y (range (* 5 height)) x (range (* 5 width))]
               [[y x] (wrap-1-9 (+ (cavern [(mod y height) (mod x width)])
                                   (quot y height)
                                   (quot x width)))]))))

(defn part-1 [cavern]
  (let [target (->> cavern keys sort last)]
    (:risk
     (search/dijkstra {:pos [0 0] :risk 0}
                      #(map (fn [pos]
                              {:pos pos :risk (+ (:risk %) (cavern pos))})
                            (grid/adjacent (:pos %) cavern))
                      :pos
                      #(= target (:pos %))
                      :risk))))

(defn part-2 [cavern]
  (part-1 (tile cavern)))

(deftest test-example
  (let [cavern (parse (str/join "\n" ["1163751742" "1381373672" "2136511328"
                                      "3694931569" "7463417111" "1319128137"
                                      "1359912421" "3125421639" "1293138521"
                                      "2311944581"]))]
    (is (= 40 (part-1 cavern)))
    (is (= 315 (part-2 cavern)))))
