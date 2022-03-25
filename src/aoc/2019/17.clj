(ns aoc.2019.17
  (:require
   [aoc.2019.intcode :as i]
   [aoc.grid :as g]
   [aoc.vector :refer [+v]]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [ascii]
  (reduce (fn [[scaffold robot y x] c]
            (case c
              \# [(conj scaffold [y x]) robot y (inc x)]
              \. [scaffold robot y (inc x)]
              \newline [scaffold robot (inc y) 0]
              \^ [scaffold {:pos [y x] :dir [-1 0]} y (inc x)]
              \< [scaffold {:pos [y x] :dir [0 -1]} y (inc x)]
              \> [scaffold {:pos [y x] :dir [0 1]} y (inc x)]
              \v [scaffold {:pos [y x] :dir [1 0]} y (inc x)]))
          [#{} nil 0 0]
          ascii))

(defn walk [[scaffold robot]]
  (loop [path [] pos (:pos robot) dir (:dir robot)]
    (let [f (+v pos dir)
          l (+v pos (g/left dir))
          r (+v pos (g/right dir))]
      (cond
        (scaffold f) (if (int? (last path))
                       (recur (update path (dec (count path)) inc) f dir)
                       (recur (conj path 1) f dir))
        (scaffold l) (recur (conj path 'L 1) l (g/left dir))
        (scaffold r) (recur (conj path 'R 1) r (g/right dir))
        :else path))))

(defn prefixes [v]
  (when (seq v)
    (cons v (prefixes (pop v)))))

(defn factor [path body]
  (loop [path path res []]
    (if (empty? path)
      res
      (if (= body (take (count body) path))
        (recur (drop (count body) path) res)
        (recur (rest path) (conj res (first path)))))))

(defn compress [path fns]
  (loop [path path main []]
    (if (empty? path)
      main
      (when-let [[fn body] (some (fn [[fn body]]
                                   (when (= body (take (count body) path))
                                     [fn body]))
                                 fns)]
        (recur (drop (count body) path) (conj main fn))))))

(defn compressions [path]
  ((fn go [path* free bound]
     (if (seq free)
       (mapcat (fn [body]
                 (go (factor path* body)
                     (rest free)
                     (assoc bound (first free) body)))
               (filter #(<= (count (str/join "," %)) 20) (prefixes path*)))
       (when-let [main (compress path bound)]
         (when (<= (count (str/join "," main)) 20)
           [(assoc bound 'Main main)]))))
   path '[A B C] {}))

(defn part-1* [[scaffold]]
  (->> scaffold
       (filter #(= 4 (count (g/adjacent % scaffold))))
       (map (fn [[y x]] (* y x)))
       (apply +)))

(defn part-1 []
  (part-1* (parse (map char (i/run-io (i/load "input/2019/17") [])))))

(defn part-2 []
  (let [mem (i/load "input/2019/17")
        compression (->> (i/run-io mem []) (map char) parse
                         walk compressions first)]
    (last (i/run-io (assoc mem 0 2)
                    (map int (format "%s\n%s\n%s\n%s\nn\n"
                                     (str/join "," (compression 'Main))
                                     (str/join "," (compression 'A))
                                     (str/join "," (compression 'B))
                                     (str/join "," (compression 'C))))))))

(defn -main []
  (i/run-interactive (assoc (i/load "input/2019/17") 0 2)))

(def small
  (str/join "\n" ["..#.........." "..#.........." "#######...###"
                  "#.#...#...#.#" "#############" "..#...#...#.."
                  "..#####...^.."]))
(def large
  (str/join "\n" ["#######...#####" "#.....#...#...#" "#.....#...#...#"
                  "......#...#...#" "......#...###.#" "......#.....#.#"
                  "^########...#.#" "......#.#...#.#" "......#########"
                  "........#...#.." "....#########.." "....#...#......"
                  "....#...#......" "....#...#......" "....#####......"]))

(deftest test-example
  (is (= 76 (part-1* (parse small))))
  (is (= "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2"
         (str/join "," (walk (parse large)))))
  (is (= '[A B C B A C]
         (compress (walk (parse large))
                   '{A [R 8 R 8] B [R 4 R 4 R 8] C [L 6 L 2]})))
  (is (some #{'{Main [A B C B A C] A [R 8 R 8] B [R 4 R 4 R 8] C [L 6 L 2]}}
            (compressions (walk (parse large))))))
