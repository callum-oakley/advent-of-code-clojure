(ns aoc.2021.19
  (:require
   [aoc.vector :refer [+v -v manhattan-distance]]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (map (fn [beacons]
         [(->> (str/replace-first beacons #".*\n" "") (re-seq #"-?\d+")
               (map read-string) (partition 3) set)
          #{[0 0 0]}])
       (str/split s #"\n\n")))

(def rotations
  (let [rx (fn [[x y z]] [x z (- y)])
        ry (fn [[x y z]] [z y (- x)])
        rz (fn [[x y z]] [y (- x) z])]
    (map #(apply comp %) (comb/subsets [rx rx rx ry ry rz]))))

(defn combine-2 [[bs0 ss0] [bs1 ss1]]
  (when-let [[[bs0 ss0] [bs1 ss1]]
             (->> (for [r rotations
                        :let [bs1 (map r bs1)
                              ss1 (map r ss1)]
                        b0 bs0
                        b1 bs1]
                    [[bs0 ss0]
                     [(set (map #(+v (-v % b1) b0) bs1))
                      (set (map #(+v (-v % b1) b0) ss1))]])
                  (filter (fn [[[bs0 ss0] [bs1 ss1]]]
                            (<= 12 (count (set/intersection bs0 bs1)))))
                  first)]
    [(set/union bs0 bs1) (set/union ss0 ss1)]))

(defn combine [scans]
  (if (= 1 (count scans))
    (first scans)
    (let [scans (sort-by (fn [[bs _]] (count bs)) > scans)]
      (println (map (fn [[bs _]] (count bs)) scans))
      (->> (for [as scans bs scans :when (not= as bs)] [as bs])
           (some (fn [[as bs]]
                   (when-let [cs (combine-2 as bs)]
                     (combine (conj (remove #{as bs} scans) cs)))))))))

(defn max-dist [ss]
  (apply max (for [s0 ss s1 ss] (manhattan-distance s0 s1))))

(defn part-1 []
  (->> "input/2021/19" slurp parse combine first count))

(defn part-2 []
  (->> "input/2021/19" slurp parse combine second max-dist))

(deftest test-example
  (let [sample
        "--- scanner 0 ---
         404,-588,-901 528,-643,409 -838,591,734 390,-675,-793 -537,-823,-458
         -485,-357,347 -345,-311,381 -661,-816,-575 -876,649,763 -618,-824,-621
         553,345,-567 474,580,667 -447,-329,318 -584,868,-557 544,-627,-890
         564,392,-477 455,729,728 -892,524,684 -689,845,-530 423,-701,434
         7,-33,-71 630,319,-379 443,580,662 -789,900,-551 459,-707,401

         --- scanner 1 ---
         686,422,578 605,423,415 515,917,-361 -336,658,858 95,138,22
         -476,619,847 -340,-569,-846 567,-361,727 -460,603,-452 669,-402,600
         729,430,532 -500,-761,534 -322,571,750 -466,-666,-811 -429,-592,574
         -355,545,-477 703,-491,-529 -328,-685,520 413,935,-424 -391,539,-444
         586,-435,557 -364,-763,-893 807,-499,-711 755,-354,-619 553,889,-390

         --- scanner 2 ---
         649,640,665 682,-795,504 -784,533,-524 -644,584,-595 -588,-843,648
         -30,6,44 -674,560,763 500,723,-460 609,671,-379 -555,-800,653
         -675,-892,-343 697,-426,-610 578,704,681 493,664,-388 -671,-858,530
         -667,343,800 571,-461,-707 -138,-166,112 -889,563,-600 646,-828,498
         640,759,510 -630,509,768 -681,-892,-333 673,-379,-804 -742,-814,-386
         577,-820,562

         --- scanner 3 ---
         -589,542,597 605,-692,669 -500,565,-823 -660,373,557 -458,-679,-417
         -488,449,543 -626,468,-788 338,-750,-386 528,-832,-391 562,-778,733
         -938,-730,414 543,643,-506 -524,371,-870 407,773,750 -104,29,83
         378,-903,-323 -778,-728,485 426,699,580 -438,-605,-362 -469,-447,-387
         509,732,623 647,635,-688 -868,-804,481 614,-800,639 595,780,-596

         --- scanner 4 ---
         727,592,562 -293,-554,779 441,611,-461 -714,465,-776 -743,427,-804
         -660,-479,-426 832,-632,460 927,-485,-438 408,393,-506 466,436,-512
         110,16,151 -258,-428,682 -393,719,612 -211,-452,876 808,-476,-593
         -575,615,604 -485,667,467 -680,325,-822 -627,-443,-432 872,-547,-609
         833,512,582 807,604,487 839,-516,451 891,-625,532 -652,-548,-490
         30,-46,-14"]
    (is (= 79 (count (first (combine (parse sample))))))
    (is (= 3621 (max-dist (second (combine (parse sample))))))))
