#!/usr/bin/env bb
(require
 '[clojure.string :as str]
  '[clojure.edn :as edn])

;; (defn re-pos [re s]
;;         (loop [m (re-matcher re s)
;;                res []]
;;           (if (.find m)
;;             (recur m (conj res [(.start m) (.group m)]))
;;             res)))

(def s (set ".0123456789"))

(defn syms? [x y len data]
  (for [yy    (range (dec y) (+ y 2))
        xx    (range (dec x) (+ x len 1))
        :when (> (count data) yy -1)
        :when (> (count (first data)) xx -1)

        :when (-> (get-in data [yy xx]) s nil?)]
    true))


(defn np [y data]
  (let [l         (get data y)
        nums      (re-seq #"\d+" l)
        positions (reduce
                   (fn [acc n]
                     (->>
                      (.indexOf l n (or (some-> (last acc) inc) 0))
                      (conj acc))) [] nums)]
    (map vector nums positions (repeat y))))

;; first
(let [data    (-> "in/3.txt"
                  (slurp)
                  (str/split #"\n"))
      preproc (for [[y l] (map vector (range) data)
                    [n x] (np y data)]
                {:y   y
                 :x   x
                 :n   (parse-long n)
                 :len (count n)})

      ]
  (reduce (fn [cnt {:keys [x y n len]}]
            (if (> (count (syms? x y len data)) 0) (+ cnt n) cnt))
          0 preproc))

;; second
(let [digits (set "0123456789")
      nbs    (fn [[x y] data]
               (for [[x y] [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
                            [(dec x) y] [(inc x) y]
                            [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]
                     :let  [s (get-in data [y x])]
                     :when (-> (digits s) nil? not)]
              [x y]))

      data  (-> "in/3.txt"
               (slurp)
               (str/split #"\n"))
      gears (for [[y l] (map vector (range) data)
                  [x s] (map vector (range) l)
                  :when (= s \*)]
              [x y])

      find-num (fn [[x y]]
                 (->> data
                      (np y)
                      (take-while (fn [[_ xx]] (>= x xx)))
                      last))
      ratios   (for [g     gears
                     :let  [n (->> (nbs g data)
                                  (map find-num)
                                  set)]
                     :when (= 2 (count n))]
               (->> n
                    (map first)
                    (map parse-long)
                    (apply *)))]

  (apply + ratios))
