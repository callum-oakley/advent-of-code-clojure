(ns aoc.2019.23
  (:require
   [aoc.2019.intcode :as i])
  (:import clojure.lang.PersistentQueue))

(defn network []
  (let [vm (i/run (i/load "input/2019/23"))]
    {:computers (mapv (fn [addr]
                        {:vm (i/>> vm addr) :inbox PersistentQueue/EMPTY})
                      (range 50))}))

(defn route [net addr packet]
  (let [{:keys [vm inbox]} (get-in net [:computers addr])]
    (assoc-in net [:computers addr] {:vm vm :inbox (conj inbox packet)})))

(defn tick [net i]
  (let [{:keys [vm inbox]} (get-in net [:computers i])]
    (case (:state vm)
      :in (if-let [[x y] (peek inbox)]
            (assoc-in net [:computers i] {:vm (-> vm (i/>> x) (i/>> y))
                                          :inbox (pop inbox)})
            (assoc-in net [:computers i] {:vm (i/>> vm -1)
                                          :inbox inbox
                                          :idle? true}))
      :out (let [[[addr x y] vm] (i/io vm [] 3)]
             (if (= addr 255)
               (-> net
                   (assoc-in [:computers i] {:vm vm :inbox inbox})
                   (assoc :nat [x y]))
               (-> net
                   (assoc-in [:computers i] {:vm vm :inbox inbox})
                   (route addr [x y])))))))

(defn part-1 []
  (loop [net (network) i 0]
    (if-let [[x y] (:nat net)]
      y
      (recur (tick net i) (mod (inc i) 50)))))

(defn part-2 []
  (loop [net (network) i 0 seen #{}]
    (if (and (:nat net) (every? :idle? (:computers net)))
      (let [[x y] (:nat net)]
        (if (seen y)
          y
          (recur (route net 0 [x y]) 0 (conj seen y))))
      (recur (tick net i) (mod (inc i) 50) seen))))
