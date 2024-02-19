#!/usr/bin/env bb
(require '[clojure
           [string :as str]
           set
           edn
           [pprint :refer [pprint]]])

(defn parse-line [s]
  (->>
    (str/split s #"\s+")
    (drop 1)
    (map parse-long)))

;; can implement better search
(defn find-first [x y]
  (loop [n 0]
    (if (> (* n (- x n)) y)
      n
      (recur (inc n)))))

(defn solutions-num [x y]
  (let [fst (find-first x y)]
    (inc (- x fst fst))))

;; 1
(let [data (-> (slurp "in/6.txt")
             (str/split #"\n")
             (->> (map parse-line)
               (apply map vector)))]
  (->> data
    (map #(apply solutions-num %))
    (apply *)
    println))
;; 1731600

;; 2
(defn parse-line [s]
  (->>
    (str/split s #"\s+")
    (drop 1)
    (str/join)
    parse-long))

(let [data (-> (slurp "in/6.txt")
             (str/split #"\n")
             (->> (map parse-line)))]
  (apply solutions-num data))

;; 40087680
