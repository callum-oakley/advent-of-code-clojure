(ns aoc.2020.24
  (:require
   [aoc.vector :refer [+v]]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (re-seq #"\w+" s))

(def dirs
  {"e" [1 0] "se" [0 1] "sw" [-1 1] "w" [-1 0] "nw" [0 -1] "ne" [1 -1]})

;; Reads an instruction to an axial coordinate
;; https://www.redblobgames.com/grids/hexagons/#coordinates-axial
(defn read-instruction [instruction]
  (->> (re-seq #"e|se|sw|w|nw|ne" instruction)
       (map dirs)
       (reduce +v [0 0])))

(defn flip [black tile]
  (if (black tile) (disj black tile) (conj black tile)))

(defn initial-state [instructions]
  (reduce flip #{} (map read-instruction instructions)))

(defn adjacent [tile]
  (map #(+v tile %) (vals dirs)))

(defn step [black]
  (set
   (filter
    (fn [tile]
      (let [c (count (filter black (adjacent tile)))]
        (if (black tile) (<= 1 c 2) (= c 2))))
    (distinct (mapcat adjacent black)))))

(defn part-1 [instructions]
  (count (initial-state instructions)))

(defn part-2 [instructions]
  (count (first (drop 100 (iterate step (initial-state instructions))))))

(def sample
  "sesenwnenenewseeswwswswwnenewsewsw neeenesenwnwwswnenewnwwsewnenwseswesw
   seswneswswsenwwnwse nwnwneseeswswnenewneswwnewseswneseene
   swweswneswnenwsewnwneneseenw eesenwseswswnenwswnwnwsewwnwsene
   sewnenenenesenwsewnenwwwse wenwwweseeeweswwwnwwe
   wsweesenenewnwwnwsenewsenwwsesesenwne neeswseenwwswnwswswnw
   nenwswwsewswnenenewsenwsenwnesesenew enewnwewneswsewnwswenweswnenwsenwsw
   sweneswneswneneenwnewenewwneswswnese swwesenesewenwneswnwwneseswwne
   enesenwswwswneneswsenwnewswseenwsese wnwnesenesenenwwnenwsewesewsesesew
   nenewswnwewswnenesenwnesewesw eneswnwswnwsenenwnwnwwseeswneewsenese
   neswnwewnwnwseenwseesewsenwsweewe wseweeenwnesenwwwswnew")

(deftest test-examples
  (is (= (part-1 (parse sample)) 10))
  (is (= (part-2 (parse sample)) 2208)))
