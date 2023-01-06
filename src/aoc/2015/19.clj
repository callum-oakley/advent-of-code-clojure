(ns aoc.2015.19
  (:require
   [aoc.search :as search]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [[r m] (str/split s #"\n\n")]
    [(map #(str/split % #" => ") (str/split-lines r)) (str/trim m)]))

(defn indices-of [s value from-index]
  (when-let [i (str/index-of s value from-index)]
    (cons i (indices-of s value (inc i)))))

(defn step [replacements molecule]
  (set (mapcat
        (fn [[from to]]
          (map
           #(str (subs molecule 0 %) to (subs molecule (+ (count from) %)))
           (indices-of molecule from 0)))
        replacements)))

(defn part-1 [[replacements molecule]]
  (count (step replacements molecule)))

(defn part-2 [[replacements molecule]]
  (let [replacements (map reverse replacements)]
    (:steps
     (search/a* {:molecule molecule
                 :steps 0}
                #(map (fn [m]
                        {:molecule m
                         :steps (inc (:steps %))})
                      (step replacements (:molecule %)))
                :molecule
                #(= "e" (:molecule %))
                :steps
                ;; This heuristic is NOT admissible, but in this case the
                ;; relaxation returns the correct result in a reasonable time.
                #(-> % :molecule count)))))

(deftest test-examples
  (let [replacements [["H" "HO"] ["H" "OH"] ["O" "HH"]]]
    (is (= 4 (part-1 [replacements "HOH"])))
    (is (= 7 (part-1 [replacements "HOHOHO"]))))
  (let [replacements [["e" "H"] ["e" "O"] ["H" "HO"] ["H" "OH"] ["O" "HH"]]]
    (is (= 3 (part-2 [replacements "HOH"])))
    (is (= 6 (part-2 [replacements "HOHOHO"])))))
