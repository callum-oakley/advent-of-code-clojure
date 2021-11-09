(ns user
  (:require
   [clojure.test :as test]
   [clojure.java.io :as io]
   [clojure.tools.namespace.repl :as repl]
   [clojure.tools.namespace.find :as find]))

(run! require (find/find-namespaces-in-dir (io/file "src")))

(def default-year 2017)

(defn run-tests* [re]
  (test/run-all-tests re))

(defn run-tests
  ([]
   (run-tests* #"aoc\..*"))
  ([year]
   (if (<= year 25)
     (run-tests default-year year)
     (run-tests* (re-pattern (format "aoc\\.%d\\..*" year)))))
  ([year day]
   (run-tests* (re-pattern (format "aoc\\.%d\\.%02d" year day)))))

(defn run
  ([]
   (run! run (range 2015 2021)))
  ([year]
   (if (<= year 25)
     (run default-year year)
     (run! #(run year %) (range 1 26))))
  ([year day]
   (if (<= year 25)
     (run default-year year day)
     (do
       (run year day 1)
       (run year day 2))))
  ([year day part]
   (let [sym (symbol (format "aoc.%d.%02d/part-%d" year day part))]
     (when-let [f (resolve sym)]
       (let [start (System/currentTimeMillis)
             res (f)
             duration (double (/ (- (System/currentTimeMillis) start) 1000))]
         (println (format "%s %7.3fs   %s" sym duration res)))))))

(defn rrr [& args]
  (repl/refresh)
  (apply run-tests args)
  (apply run args))

(defn log []
  (spit "results.log" (with-out-str (run))))
