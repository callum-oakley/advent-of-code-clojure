(ns aoc.2022.19
  (:require
   [aoc.map :refer [+m -m]]
   [aoc.search :as search]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string) (partition 7)
       (map (fn [[_ ore-ore cla-ore obs-ore obs-cla geo-ore geo-obs]]
              {:ore {:ore ore-ore}
               :cla {:ore cla-ore}
               :obs {:ore obs-ore :cla obs-cla}
               :geo {:ore geo-ore :obs geo-obs}}))))

(defn quality [blueprint time]
  (search/branch-and-bound
   {:minute 1
    :robots {:ore 1 :cla 0 :obs 0 :geo 0}
    :resources {:ore 0 :cla 0 :obs 0 :geo 0}
    :skipped #{}}
   (fn [{:keys [minute robots resources skipped]}]
     (when (<= minute time)
       (let [affordable? (fn [robot]
                           (every? (fn [[resource n]]
                                     (<= n (resources resource)))
                                   (blueprint robot)))]
         ;; Always build a geo robot if we can afford to
         (if (affordable? :geo)
           [{:minute (inc minute)
             :robots (update robots :geo inc)
             :resources (+m (-m resources (blueprint :geo)) robots)
             :skipped #{}}]
           (let [options (filter affordable? [:ore :cla :obs])]
             ;; It might be better to wait, but if we do take note of what we
             ;; skipped building because there's no point building them next
             ;; time.
             (cons
              {:minute (inc minute)
               :robots robots
               :resources (+m resources robots)
               :skipped (set options)}
              (map (fn [robot]
                     {:minute (inc minute)
                      :robots (update robots robot inc)
                      :resources (+m (-m resources (blueprint robot)) robots)
                      :skipped #{}})
                   ;; Don't build robots for resources we already have a lot of.
                   ;; Where "a lot" is... 16.
                   (remove #(or (skipped %) (< 16 (resources %)))
                           options))))))))
   identity
   #(:geo (:resources %))
   ;; A very crude upper bound assuming we can make a geo robot in every
   ;; remaining minute.
   (fn [{:keys [minute robots resources]}]
     (let [m (- (inc time) minute)]
       (+ (:geo resources) (* m (:geo robots)) (/ (* (dec m) m) 2))))))

(defn part-1 [blueprints]
  (apply + (map-indexed (fn [i blueprint] (* (inc i) (quality blueprint 24)))
                        blueprints)))

(defn part-2 [blueprints]
  (apply * (map (fn [blueprint] (quality blueprint 32)) (take 3 blueprints))))

(def example
  "1 4 2 3 14 2 7 2 2 3 3 8 3 12")

(deftest test-example
  (is (= 33 (part-1 (parse example))))
  (is (= 56 (quality (first (parse example)) 32)))
  (is (= 62 (quality (second (parse example)) 32))))
