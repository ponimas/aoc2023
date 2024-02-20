#!/usr/bin/env bb
(require '[clojure
           [string :as str]
           set
           [pprint :refer [pprint]]])





;; a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.

(defn weights [hand]
  (let [[f s & _] (->>
                    hand
                    (group-by identity)
                    vals
                    (map count)
                    (sort-by -))]
    [f (or s 0)]))


(def wmap {\A 14
           \K 13
           \Q 12
           \J 11
           \T 10
           \9 9
           \8 8
           \7 7
           \6 6
           \5 5
           \4 4
           \3 3
           \2 2})

(defn cmp [h1 h2]
  (let [[f1 s1] (weights h1)
        [f2 s2] (weights h2)]
    (cond
      (< f1 f2) -1
      (> f1 f2) 1
      (< s1 s2) -1
      (> s1 s2) 1
      :default
      (->>
        (map vector h1 h2)
        (map (fn [[a b]] (compare (wmap a) (wmap b))))
        (remove zero?)
        first))))

(-> (slurp "in/7.txt")
  (str/split #"\n")
  (->>
    (map #(str/split % #"\s+"))
    (sort-by first cmp)
    (map second)
    (map parse-long)
    (map #(* (inc %1) %2) (range))
    (apply +)))

;; 250957639
