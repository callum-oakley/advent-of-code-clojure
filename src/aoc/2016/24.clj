(ns aoc.2016.24
  (:require
   [aoc.search :as search]
   [aoc.vector :refer [+v]]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> (str/split-lines s)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c] [[x y] c]) (str/trim line))))
       (apply concat)
       (remove (fn [[_ c]] (= \# c)))
       (reduce (fn [state [pos c]]
                 (let [state (update state :paths conj pos)]
                   (if (not= \. c)
                     (update state :targets assoc (- (int c) (int \0)) pos)
                     state)))
               {:paths #{} :targets {}})))

(defn distance [paths start goal]
  (:steps (search/bfs {:pos start :steps 0}
                      (fn [{:keys [pos steps]}]
                        (->> [[1 0] [-1 0] [0 1] [0 -1]]
                             (map #(+v % pos))
                             (filter paths)
                             (map (fn [pos] {:pos pos :steps (inc steps)}))))
                      #(= goal (:pos %))
                      :pos)))

(defn distances [{:keys [paths targets]}]
  (reduce (fn [distances [a b]]
            (let [d (distance paths (targets a) (targets b))]
              (-> distances (assoc [a b] d) (assoc [b a] d))))
          {}
          (for [a (keys targets) b (range a)] [a b])))

(defn part-* [end {:keys [targets] :as state}]
  (let [distances (distances state)]
    (->> targets keys (remove #{0}) comb/permutations
         (map #(->> (concat [0] % end)
                    (partition 2 1)
                    (map distances)
                    (apply +)))
         (apply min))))

(defn part-1 []
  (part-* nil (parse (slurp "input/2016/24"))))

(defn part-2 []
  (part-* [0] (parse (slurp "input/2016/24"))))

(deftest test-part-*
  (is (= 14 (part-* nil (parse "###########
                                #0.1.....2#
                                #.#######.#
                                #4.......3#
                                ###########")))))
