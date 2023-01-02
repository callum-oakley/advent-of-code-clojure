(ns aoc.2016.08
  (:require
   [aoc.ocr :as ocr]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse [s]
  (map (fn [line]
         [(cond
            (str/starts-with? line "rect") 'rect
            (str/starts-with? line "rotate row") 'rotate-row
            (str/starts-with? line "rotate col") 'rotate-col)
          (map read-string (re-seq #"\d+" line))])
       (str/split-lines s)))

(defn rect [pixels w h]
  (set/union pixels (set (for [x (range w) y (range h)] [x y]))))

(defn rotate-row [pixels row n]
  (set (map (fn [[x y]] (if (= y row) [(mod (+ n x) 50) y] [x y])) pixels)))

(defn rotate-col [pixels col n]
  (set (map (fn [[x y]] (if (= x col) [x (mod (+ n y) 6)] [x y])) pixels)))

(defn part-* [instructions]
  (reduce (fn [pixels [op args]]
            (apply (case op
                     rect rect
                     rotate-row rotate-row
                     rotate-col rotate-col)
                   pixels
                   args))
          #{}
          instructions))

(defn part-1 [instructions]
  (count (part-* instructions)))

(defn part-2 [instructions]
  (ocr/parse (ocr/draw (part-* instructions))))
