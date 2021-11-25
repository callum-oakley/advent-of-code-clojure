(ns aoc.2020.04
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [deftest is]]))

(defn parse-kv [s]
  (let [[k v] (str/split s #":")]
    [(keyword k) v]))

(defn parse-passport [s]
  (into {} (map parse-kv (str/split s #"\s+"))))

(defn parse [s]
  (map parse-passport (str/split s #"\n\n")))

(def data
  (parse (slurp "input/2020/04")))

(defn valid-1? [passport]
  (set/subset?
   #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
   (set (keys passport))))

(defn valid-2? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and
   (<= 1920 (read-string byr) 2002)
   (<= 2010 (read-string iyr) 2020)
   (<= 2020 (read-string eyr) 2030)
   (let [[_ n units] (re-matches #"(\d+)(..)" hgt)]
     (case units
       "cm" (<= 150 (read-string n) 193)
       "in" (<= 59 (read-string n) 76)
       false))
   (re-matches #"#[0-9a-f]{6}" hcl)
   (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
   (re-matches #"\d{9}" pid)))

(defn part-1
  ([] (part-1 data))
  ([passports] (count (filter valid-1? passports))))

(defn part-2
  ([] (part-2 data))
  ([passports]
   (count (filter (every-pred valid-1? valid-2?) passports))))

(def sample
  (parse "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
          byr:1937 iyr:2017 cid:147 hgt:183cm

          iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
          hcl:#cfa07d byr:1929

          hcl:#ae17e1 iyr:2013
          eyr:2024
          ecl:brn pid:760753108 byr:1931
          hgt:179cm

          hcl:#cfa07d eyr:2025 pid:166559648
          iyr:2011 ecl:brn hgt:59in"))

(deftest test-examples
  (is (= (part-1 sample) 2))
  (is (not (valid-2?
            (parse-passport
             "eyr:1972 cid:100
              hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"))))
  (is (not (valid-2?
            (parse-passport
             "iyr:2019
              hcl:#602927 eyr:1967 hgt:170cm
              ecl:grn pid:012533040 byr:1946"))))
  (is (not (valid-2?
            (parse-passport
             "hcl:dab227 iyr:2012
              ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"))))
  (is (not (valid-2?
            (parse-passport
             "hgt:59cm ecl:zzz
              eyr:2038 hcl:74454a iyr:2023
              pid:3556412378 byr:2007"))))
  (is (valid-2?
       (parse-passport
        "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
         hcl:#623a2f")))
  (is (valid-2?
       (parse-passport
        "eyr:2029 ecl:blu cid:129 byr:1989
         iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm")))
  (is (valid-2?
       (parse-passport
        "hcl:#888785
         hgt:164cm byr:2001 iyr:2015 cid:88
         pid:545766238 ecl:hzl
         eyr:2022")))
  (is (valid-2?
       (parse-passport
        "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
         hcl:#623a2f"))))
