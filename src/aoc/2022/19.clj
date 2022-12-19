(ns aoc.2022.19 
  (:require
   [aoc.search :as search]
   [clojure.test :refer [deftest is]]))

(defn parse [s]
  (->> s (re-seq #"\d+") (map read-string) (partition 7)
       (map (fn [[_ ore-ore cla-ore obs-ore obs-cla geo-ore geo-obs]]
              {:ore {:ore ore-ore}
               :cla {:ore cla-ore}
               :obs {:ore obs-ore :cla obs-cla}
               :geo {:ore geo-ore :obs geo-obs}}))))

;; Improves on brute force by:
;;   - if we can afford a geo robot, always build it immediately
;;   - if we opted not to build a robot we could have last turn, don't build it
;;     this turn (it would have been strictly better to build it last turn)
;;   - don't build a robot if we have more than 16 of its resource or more than
;;     7 robots of the same type already (found by trial and error)
(defn quality [blueprint time]
  (->>
   (search/dft
    {:minute 1
     :robots {:ore 1 :cla 0 :obs 0 :geo 0}
     :resources {:ore 0 :cla 0 :obs 0 :geo 0}
     :skipped #{}}
    (fn [{:keys [minute robots resources skipped]}]
      (when (<= minute time)
        (let [m+ (fn [a b] (merge-with + a b))
              m- (fn [a b] (merge-with - a b))
              affordable? (fn [robot]
                            (every? (fn [[resource n]]
                                      (<= n (resources resource)))
                                    (blueprint robot)))]
          (if (affordable? :geo)
            [{:minute (inc minute)
              :robots (update robots :geo inc)
              :resources (m+ (m- resources (blueprint :geo)) robots)
              :skipped #{}}]
            (let [options (filter affordable? [:ore :cla :obs])]
              (cons
               {:minute (inc minute)
                :robots robots
                :resources (m+ resources robots)
                :skipped (set options)}
               (map (fn [robot]
                      {:minute (inc minute)
                       :robots (update robots robot inc)
                       :resources (m+ (m- resources (blueprint robot)) robots)
                       :skipped #{}})
                    (remove #(or (skipped %)
                                 (< 16 (resources %)) (< 7 (robots %)))
                            options))))))))
    identity)
   (filter #(< time (:minute %)))
   (map #(:geo (:resources %)))
   (apply max)))

(defn part-1 [blueprints]
  (apply + (map-indexed (fn [i blueprint] (* (inc i) (quality blueprint 24)))
                        blueprints)))

(def example
  "1 4 2 3 14 2 7 2 2 3 3 8 3 12")

(deftest test-example
  (is (= 33 (part-1 (parse example))))
  #_(is (= 56 (quality (first (parse example)) 32)))
  #_(is (= 62 (quality (second (parse example)) 32))))
