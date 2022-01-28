(ns aoc.2020.12
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (str/split-lines (slurp "input/2020/12")))

(defn parse [instructions]
  (map #(vector (first %) (read-string (subs % 1))) instructions))

(def compass
  {\N [0 1] \S [0 -1] \E [1 0] \W [-1 0]})

(defn move [pos dir dist]
  (map + pos (map #(* dist %) dir)))

(defn turn [dir deg]
  (nth
   (iterate (fn [[x y]] [(- y) x]) dir)
   (/ (mod deg 360) 90)))

(defn step-1 [{:keys [pos dir]} [op arg]]
  (condp contains? op
    #{\N \S \E \W} {:pos (move pos (compass op) arg) :dir dir}
    #{\L \R}       {:pos pos :dir (turn dir (case op \L arg \R (- arg)))}
    #{\F}          {:pos (move pos dir arg) :dir dir}))

(defn step-2 [{:keys [pos dir] :as ship} [op arg]]
  (if (#{\N \S \E \W} op)
    {:pos pos :dir (move dir (compass op) arg)}
    (step-1 ship [op arg])))

(defn part-1
  ([] (part-1 (parse data)))
  ([instructions]
   (let [{[x y] :pos} (reduce step-1 {:pos [0 0] :dir [1 0]} instructions)]
     (+ (abs x) (abs y)))))

(defn part-2
  ([] (part-2 (parse data)))
  ([instructions]
   (let [{[x y] :pos} (reduce step-2 {:pos [0 0] :dir [10 1]} instructions)]
     (+ (abs x) (abs y)))))

(def sample
  ["F10" "N3" "F7" "R90" "F11"])

(deftest test-examples
  (is (= (part-1 (parse sample)) 25))
  (is (= (part-2 (parse sample)) 286)))
