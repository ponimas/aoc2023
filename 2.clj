#!/usr/bin/env bb
(require
 '[clojure.string :as str]
 '[clojure.edn :as edn])

(def games (for [game (->> "in/2.txt"
                           clojure.java.io/reader
                           line-seq)]
             (let [n     (->> game (re-find #"^Game (\d+):") last edn/read-string)
                   moves (for [s    (re-seq #"\d+ \w+" game)
                               :let [[n c] (str/split s #"\s")]]
                           [(edn/read-string n) c])]
               [n moves])))
;; 1

(def contained {"red" 12 "green" 13 "blue" 14})

(reduce
 (fn [v [n data]]
   (if (every? (fn [[i c]] (>= (contained c) i)) data)
     (+ n v)
     v)) 0 games)

;; 2

(defn find-mins [data]
  (reduce (fn [m [i c]]
            (update m c #(max i (or % 0)))) {} data))

(->>
 (for [[_ d] games
       :let  [mins (find-mins d)]]
   (reduce-kv (fn [c _ i] (* c i)) 1 mins))
 (apply +))
