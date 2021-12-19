(ns aoc.2021.19
  (:require
   [aoc.vector :refer [+v -v manhattan-distance]]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

;; Taking combinations of rotations around the axes generates all 24 rotations,
;; but with redundancy, so filter them down to just the 24 we want.
(def rotations
  (let [rx (fn [[x y z]] [x z (- y)])
        ry (fn [[x y z]] [z y (- x)])
        rz (fn [[x y z]] [y (- x) z])]
    (->> [rx rx rx ry ry ry rz rz rz] comb/subsets
         (map #((apply comp %) [1 2 3]))
         distinct
         (map #(eval
                (list 'fn '[[x y z]]
                      (mapv '{1 x 2 y 3 z -1 (- x) -2 (- y) -3 (- z)} %)))))))

(defn fingerprint [beacons]
  (->> (comb/combinations beacons 2)
       (map #(apply manhattan-distance %))
       frequencies))

(defn parse [s]
  (map (fn [beacons]
         (let [beacons (->> (str/replace-first beacons #".*\n" "")
                            (re-seq #"-?\d+") (map read-string) (partition 3)
                            set)]
           {:beacons beacons
            :scanners #{[0 0 0]}
            :fingerprint (fingerprint beacons)}))
       (str/split s #"\n\n")))

(defn fingerprints-match? [f0 f1]
  (<= 66 ; 12 choose 2
      (apply + (keep (fn [[k v0]] (when-let [v1 (f1 k)] (min v0 v1))) f0))))

(defn fix [relative fixed]
  (when (fingerprints-match? (:fingerprint relative) (:fingerprint fixed))
    (first
     (for [r rotations
           :let [rbs (map r (:beacons relative))]
           fb (:beacons fixed)
           rb rbs
           :let [rbs (set (map #(+v (-v % rb) fb) rbs))]
           :when (<= 12 (count (set/intersection rbs (:beacons fixed))))]
       [relative
        {:beacons rbs
         :scanners (set (map #(+v (-v % rb) fb) (map r (:scanners relative))))
         :fingerprint (:fingerprint relative)}]))))

(defn combine [scans]
  (loop [fixed #{(first scans)}
         relative (set (rest scans))]
    (if (seq relative)
      (let [[r f] (first (remove nil? (for [r relative f fixed] (fix r f))))]
        (recur (conj fixed f) (disj relative r)))
      (apply merge-with set/union (map #(dissoc % :fingerprint) fixed)))))

(defn max-dist [scanners]
  (apply max (for [s0 scanners s1 scanners] (manhattan-distance s0 s1))))

(defn part-1 []
  (->> "input/2021/19" slurp parse combine :beacons count))

(defn part-2 []
  (->> "input/2021/19" slurp parse combine :scanners max-dist))

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
    (is (= 79 (count (:beacons (combine (parse sample))))))
    (is (= 3621 (max-dist (:scanners (combine (parse sample))))))))
