(ns aoc.2021.16
  (:require
   [clojure.test :refer [deftest is]]))

(defn hex->bin [hex]
  (mapcat (fn [c]
            (map #(bit-test (Character/digit c 16) %) (range 3 -1 -1)))
          hex))

(defn bin->int [bin]
  (read-string (apply str "2r" (map {false 0 true 1} bin))))

(declare decode*)

(defn decode-literal [bin]
  (loop [literal [] bin bin]
    (let [[more? v0 v1 v2 v3 & bin] bin]
      (if more?
        (recur (conj literal v0 v1 v2 v3) bin)
        [(bin->int (conj literal v0 v1 v2 v3)) bin]))))

(defn decode-subpackets-by-count [bin]
  (let [subpacket-count (bin->int (take 11 bin))]
    (loop [subpackets [] bin (drop 11 bin)]
      (if (< (count subpackets) subpacket-count)
        (let [[subpacket bin] (decode* bin)]
          (recur (conj subpackets subpacket) bin))
        [subpackets bin]))))

(defn decode-subpackets-by-length [bin]
  (let [length (bin->int (take 15 bin))]
    [(loop [subpackets [] bin (take length (drop 15 bin))]
       (if (seq bin)
         (let [[subpacket bin] (decode* bin)]
           (recur (conj subpackets subpacket) bin))
         subpackets))
     (drop length (drop 15 bin))]))

(defn decode-operator [[by-count? & bin]]
  (if by-count?
    (decode-subpackets-by-count bin)
    (decode-subpackets-by-length bin)))

(defn decode* [[v0 v1 v2 t0 t1 t2 & bin]]
  (let [version (bin->int [v0 v1 v2])
        type (bin->int [t0 t1 t2])
        [value bin] (if (= 4 type)
                      (decode-literal bin)
                      (decode-operator bin))]
    [{:version version :type type :value value} bin]))

(defn decode [bin]
  (first (decode* bin)))

(defn version-sum [packet]
  (+ (:version packet)
     (if (= 4 (:type packet))
       0
       (apply + (map version-sum (:value packet))))))

(defn eval-packet [packet]
  (if (= 4 (:type packet))
    (:value packet)
    (let [res (apply (case (:type packet) 0 + 1 * 2 min 3 max 5 > 6 < 7 =)
                     (map eval-packet (:value packet)))]
      (case res true 1 false 0 res))))

(defn part-1 [hex]
  (->> hex hex->bin decode version-sum))

(defn part-2 [hex]
  (->> hex hex->bin decode eval-packet))

(deftest test-example
  (is (= {:version 6 :type 4 :value 2021} (decode (hex->bin "D2FE28"))))
  (is (= [1 2 3] (map :value (:value (decode (hex->bin "EE00D40C823060"))))))
  (is (= 16 (part-1 "8A004A801A8002F478")))
  (is (= 12 (part-1 "620080001611562C8802118E34")))
  (is (= 23 (part-1 "C0015000016115A2E0802F182340")))
  (is (= 31 (part-1 "A0016C880162017C3686B18A3D4780")))
  (is (= 3 (part-2 "C200B40A82")))
  (is (= 54 (part-2 "04005AC33890")))
  (is (= 7 (part-2 "880086C3E88112")))
  (is (= 9 (part-2 "CE00C43D881120")))
  (is (= 1 (part-2 "D8005AC2A8F0")))
  (is (= 0 (part-2 "F600BC2D8F")))
  (is (= 0 (part-2 "9C005AC2F8F0")))
  (is (= 1 (part-2 "9C0141080250320F1802104A08"))))
