(ns aoc.2015.19
  (:require
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

(defn part-1* [replacements molecule]
  (count (step replacements molecule)))

(defn part-2* [replacements molecule]
  (let [replacements (map reverse replacements)]
    (loop [molecule molecule
           steps 0]
      (if (= "e" molecule)
        steps
        ;; Choosing the most promising branch (the shortest molecule) at each
        ;; step gets us the optimal solution in this case. Good enough.
        (recur (apply min-key count (step replacements molecule))
               (inc steps))))))

(defn part-1 []
  (apply part-1* (parse (slurp "input/2015/19"))))

(defn part-2 []
  (apply part-2* (parse (slurp "input/2015/19"))))

(deftest test-part-1*
  (let [replacements [["H" "HO"] ["H" "OH"] ["O" "HH"]]]
    (is (= 4 (part-1* replacements "HOH")))
    (is (= 7 (part-1* replacements "HOHOHO")))))

(deftest test-part-2*
  (let [replacements [["e" "H"] ["e" "O"] ["H" "HO"] ["H" "OH"] ["O" "HH"]]]
    (is (= 3 (part-2* replacements "HOH")))
    (is (= 6 (part-2* replacements "HOHOHO")))))

(deftest test-answers
  (is (= 535 (part-1)))
  (is (= 212 (part-2))))
