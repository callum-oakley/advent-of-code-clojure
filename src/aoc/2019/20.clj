(ns aoc.2019.20
  (:require
   [aoc.grid :as g]
   [aoc.vector :refer [+v]]
   [aoc.search :as search]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (let [g (g/parse s)
        maze (set (keep (fn [[pos tile]] (when (= \. tile) pos)) g))
        label (fn [pos]
                (some (fn [dir]
                        (let [a (+v dir pos) b (+v dir a)]
                          (when (and (java.lang.Character/isLetter (g a))
                                     (java.lang.Character/isLetter (g b)))
                            (apply str (map g (sort [a b]))))))
                      [g/north g/east g/south g/west]))]
    [maze
     (reduce (fn [portals pos]
               (if-let [label (label pos)]
                 (if-let [pos* (portals label)]
                   (-> portals (dissoc portals label) (assoc pos pos* pos* pos))
                   (assoc portals label pos))
                 portals))
             {}
             maze)]))

(defn part-1* [[maze portals]]
  (:steps (search/bfs {:pos (portals "AA") :steps 0}
                      #(map (fn [pos] {:pos pos :steps (inc (:steps %))})
                            (if-let [pos (portals (:pos %))]
                              (cons pos (g/adjacent (:pos %) maze))
                              (g/adjacent (:pos %) maze)))
                      :pos
                      #(= (portals "ZZ") (:pos %)))))

(defn part-2* [[maze portals]]
  (let [min-x (apply min (map first maze)) min-y (apply min (map second maze))
        max-x (apply max (map first maze)) max-y (apply max (map second maze))
        in? (fn [[x y]] (and (< min-x x max-x) (< min-y y max-y)))]
    (:steps
     (search/bfs {:pos (portals "AA") :level 0 :steps 0}
                 (fn [state]
                   (remove #(or (nil? %) (neg? (:level %)))
                           (cons (when-let [pos (portals (:pos state))]
                                   {:pos pos
                                    :level (if (in? (:pos state))
                                             (inc (:level state))
                                             (dec (:level state)))
                                    :steps (inc (:steps state))})
                                 (map (fn [pos]
                                        {:pos pos
                                         :level (:level state)
                                         :steps (inc (:steps state))})
                                      (g/adjacent (:pos state) maze)))))
                 (juxt :pos :level)
                 #(and (zero? (:level %)) (= (portals "ZZ") (:pos %)))))))

(defn part-1 []
  (->> "input/2019/20" slurp parse part-1*))

(defn part-2 []
  (->> "input/2019/20" slurp parse part-2*))

(def examples
  (mapv
   #(str/join "\n" %)
   [["         A           " "         A           " "  #######.#########  "
     "  #######.........#  " "  #######.#######.#  " "  #######.#######.#  "
     "  #######.#######.#  " "  #####  B    ###.#  " "BC...##  C    ###.#  "
     "  ##.##       ###.#  " "  ##...DE  F  ###.#  " "  #####    G  ###.#  "
     "  #########.#####.#  " "DE..#######...###.#  " "  #.#########.###.#  "
     "FG..#########.....#  " "  ###########.#####  " "             Z       "
     "             Z       "]
    ["                   A               " "                   A               "
     "  #################.#############  " "  #.#...#...................#.#.#  "
     "  #.#.#.###.###.###.#########.#.#  " "  #.#.#.......#...#.....#.#.#...#  "
     "  #.#########.###.#####.#.#.###.#  " "  #.............#.#.....#.......#  "
     "  ###.###########.###.#####.#.#.#  " "  #.....#        A   C    #.#.#.#  "
     "  #######        S   P    #####.#  " "  #.#...#                 #......VT"
     "  #.#.#.#                 #.#####  " "  #...#.#               YN....#.#  "
     "  #.###.#                 #####.#  " "DI....#.#                 #.....#  "
     "  #####.#                 #.###.#  " "ZZ......#               QG....#..AS"
     "  ###.###                 #######  " "JO..#.#.#                 #.....#  "
     "  #.#.#.#                 ###.#.#  " "  #...#..DI             BU....#..LF"
     "  #####.#                 #.#####  " "YN......#               VT..#....QG"
     "  #.###.#                 #.###.#  " "  #.#...#                 #.....#  "
     "  ###.###    J L     J    #.#.###  " "  #.....#    O F     P    #.#...#  "
     "  #.###.#####.#.#####.#####.###.#  " "  #...#.#.#...#.....#.....#.#...#  "
     "  #.#####.###.###.#.#.#########.#  " "  #...#.#.....#...#.#.#.#.....#.#  "
     "  #.###.#####.###.###.#.#.#######  " "  #.#.........#...#.............#  "
     "  #########.###.###.#############  " "           B   J   C               "
     "           U   P   P               "]
    ["             Z L X W       C                 "
     "             Z P Q B       K                 "
     "  ###########.#.#.#.#######.###############  "
     "  #...#.......#.#.......#.#.......#.#.#...#  "
     "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
     "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
     "  #.###.#######.###.###.#.###.###.#.#######  "
     "  #...#.......#.#...#...#.............#...#  "
     "  #.#########.#######.#.#######.#######.###  "
     "  #...#.#    F       R I       Z    #.#.#.#  "
     "  #.###.#    D       E C       H    #.#.#.#  "
     "  #.#...#                           #...#.#  "
     "  #.###.#                           #.###.#  "
     "  #.#....OA                       WB..#.#..ZH"
     "  #.###.#                           #.#.#.#  "
     "CJ......#                           #.....#  "
     "  #######                           #######  "
     "  #.#....CK                         #......IC"
     "  #.###.#                           #.###.#  "
     "  #.....#                           #...#.#  "
     "  ###.###                           #.#.#.#  "
     "XF....#.#                         RF..#.#.#  "
     "  #####.#                           #######  "
     "  #......CJ                       NM..#...#  "
     "  ###.#.#                           #.###.#  "
     "RE....#.#                           #......RF"
     "  ###.###        X   X       L      #.#.#.#  "
     "  #.....#        F   Q       P      #.#.#.#  "
     "  ###.###########.###.#######.#########.###  "
     "  #.....#...#.....#.......#...#.....#.#...#  "
     "  #####.#.###.#######.#######.###.###.#.#.#  "
     "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
     "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
     "  #.......#.....#.#...#...............#...#  "
     "  #############.#.#.###.###################  "
     "               A O F   N                     "
     "               A A D   M                     "]]))

(deftest test-examples
  (is (= 23 (part-1* (parse (examples 0)))))
  (is (= 58 (part-1* (parse (examples 1)))))
  (is (= 26 (part-2* (parse (examples 0)))))
  (is (= 396 (part-2* (parse (examples 2))))))
