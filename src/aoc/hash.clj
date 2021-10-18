(ns aoc.hash
  (:import java.security.MessageDigest))

(defn md5 [bs]
  (.digest (MessageDigest/getInstance "MD5") bs))

