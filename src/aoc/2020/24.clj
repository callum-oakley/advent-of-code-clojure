(ns aoc.2020.24
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def data
  (str/split-lines (slurp "input/2020/24")))

(def dirs
  {"e" [1 0] "se" [0 1] "sw" [-1 1] "w" [-1 0] "nw" [0 -1] "ne" [1 -1]})

;; Reads an instruction to an axial coordinate
;; https://www.redblobgames.com/grids/hexagons/#coordinates-axial
(defn read-instruction [instruction]
  (->> (re-seq #"e|se|sw|w|nw|ne" instruction)
       (map dirs)
       (reduce #(mapv + %1 %2) [0 0])))

(defn flip [black tile]
  (if (black tile) (disj black tile) (conj black tile)))

(defn initial-state [instructions]
  (reduce flip #{} (map read-instruction instructions)))

(defn adjacent [tile]
  (map #(mapv + tile %) (vals dirs)))

(defn step [black]
  (set
   (filter
    (fn [tile]
      (let [c (count (filter black (adjacent tile)))]
        (if (black tile) (<= 1 c 2) (= c 2))))
    (distinct (mapcat adjacent black)))))

(defn part-1
  ([] (part-1 data))
  ([instructions]
   (count (initial-state instructions))))

(defn part-2
  ([] (part-2 data))
  ([instructions]
   (count (first (drop 100 (iterate step (initial-state instructions)))))))

(def sample
  ["sesenwnenenewseeswwswswwnenewsewsw"
   "neeenesenwnwwswnenewnwwsewnenwseswesw"
   "seswneswswsenwwnwse"
   "nwnwneseeswswnenewneswwnewseswneseene"
   "swweswneswnenwsewnwneneseenw"
   "eesenwseswswnenwswnwnwsewwnwsene"
   "sewnenenenesenwsewnenwwwse"
   "wenwwweseeeweswwwnwwe"
   "wsweesenenewnwwnwsenewsenwwsesesenwne"
   "neeswseenwwswnwswswnw"
   "nenwswwsewswnenenewsenwsenwnesesenew"
   "enewnwewneswsewnwswenweswnenwsenwsw"
   "sweneswneswneneenwnewenewwneswswnese"
   "swwesenesewenwneswnwwneseswwne"
   "enesenwswwswneneswsenwnewswseenwsese"
   "wnwnesenesenenwwnenwsewesewsesesew"
   "nenewswnwewswnenesenwnesewesw"
   "eneswnwswnwsenenwnwnwwseeswneewsenese"
   "neswnwewnwnwseenwseesewsenwsweewe"
   "wseweeenwnesenwwwswnew"])

(deftest test-part-1
  (is (= (part-1 sample) 10))
  (is (= (part-1) 549)))

(deftest test-part-2
  (is (= (part-2 sample) 2208))
  (is (= (part-2) 4147)))
