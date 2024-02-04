#!/usr/bin/env bb
(require
 '[clojure.string :as str]
 '[clojure.set :as set]
 '[clojure.edn :as edn])

(defn parse-int [n] (Integer/parseInt n))

(defn parse-card [l]
  (let [[_ w h] (str/split l #"(:\s+| \|\s+)")
        w       (map parse-int (str/split w #"\s+"))
        h       (map parse-int (str/split h #"\s+"))]
    [w h]))

(defn check-win [[w h]]
  (let [c (count
           (set/intersection
            (into #{} w)
            (into #{} h)))]
    (when (pos? c) c)))

(let [lines        (-> (slurp "in/4.txt") (str/split #"\n"))
      parsed-cards (map parse-card lines)]
  (->> parsed-cards
       (map check-win)
       (filter some?)
       (map (fn [x] (clojure.math/pow 2 (dec x))))
       (apply +)
       int
       ))
;; 21821


(let [lines            (-> (slurp "in/4.txt") (str/split #"\n"))
      parsed-cards     (mapv parse-card lines)
      cards-count-init (vec (repeat (count parsed-cards) 1))]
  (->> parsed-cards
       (reduce-kv
        (fn [cards-count i card]
          (if-let [w (check-win card)]
            (let [increment         (get cards-count i)
                  application-range (range (inc i) (+ i w 1))]
              (reduce
               (fn [v j] (update v j + increment))
               cards-count application-range))
            cards-count)) cards-count-init)
       (apply +)))

;; 5539496
