#!/usr/bin/env bb
(require '[clojure.string :as str])

(def replacements
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})

(-> (slurp "in/1.txt")
    (str/replace #"[a-z]" "")
    (str/split #"\n")
    (->>
     (map #(str (first %) (last %)))
     (map clojure.edn/read-string)
     (apply +))
    println)

(defn transform [s]
  (->>
   (for [i (range (count s))] (subs s i))
   (map #(re-find #"one|two|three|four|five|six|seven|eight|nine|\d" %))
   (filter some?)
   (map #(get replacements % %))))

(->>
 "in/1.txt"
 clojure.java.io/reader
 line-seq
 (map transform)
 (map #(str (first %) (last %)))
 (map clojure.edn/read-string)
 (apply +))
