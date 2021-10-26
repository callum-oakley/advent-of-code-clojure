(ns aoc.hash
  (:import java.security.MessageDigest))

(defn md5 [bs]
  (.digest (MessageDigest/getInstance "MD5") bs))

(defn- hex-ch [b]
  (if (< b 10) (+ (int \0) b) (+ (int \a) (- b 10))))

(defn hex-bytes [bs]
  (let [buffer (byte-array (* 2 (count bs)))]
    (reduce (fn [i b]
              (aset-byte buffer i (hex-ch (bit-and 0x0f (bit-shift-right b 4))))
              (aset-byte buffer (inc i) (hex-ch (bit-and 0x0f b)))
              (+ 2 i))
            0
            bs)
    buffer))

(defn hex [bs]
  (String. (hex-bytes bs)))
