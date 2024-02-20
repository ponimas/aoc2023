#!/usr/bin/env bb
(require '[clojure
           [string :as str]
           set
           [pprint :refer [pprint]]])

;; Comparator
;; a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.

(defn weights [hand]
  (let [[f s & _] (->>
                    hand
                    (group-by identity)
                    vals
                    (map count)
                    (sort-by -))]
    [f (or s 0)]))

(def wmap
  (into {}
    (map vector
      (reverse "AKQJT98765432")
      (range))))

(defn cmp [weights wmap h1 h2]
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

(defn calc-result [l]
  (->> l
    (map second)
    (map parse-long)
    (map #(* (inc %1) %2) (range))
    (apply +)))

(-> (slurp "in/7.txt")
  (str/split #"\n")
  (->>
    (map #(str/split % #"\s+"))
    (sort-by first (partial cmp weights wmap))
    calc-result))

;; 250957639

;;

(def jwmap (assoc wmap \J -1))

(defn jweights [hand]
  (let [{j \J :or {j 0} :as weights}
        (as-> hand $
          (group-by identity $)
          (update-vals $ count)
          (into
            (sorted-map-by (fn [k1 k2]
                             (compare [(get $ k2) k2] [(get $ k1) k1])))
            $))
        weights   (dissoc weights \J)
        top       (ffirst weights)
        [f s & _] (if (not-empty weights)
                    (vals (update weights top + j))
                    ;; corner case when the hand is "JJJJJ"
                    [5 0])]
    [f (or s 0)]))

(-> (slurp "in/7.txt")
  (str/split #"\n")
  (->>
    (map #(str/split % #"\s+"))
    (sort-by first (partial cmp jweights jwmap))
    calc-result))

;; 251515496
