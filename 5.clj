#!/usr/bin/env bb
(require '[clojure
           [string :as str]
           set
           edn
           [pprint :refer [pprint]]])

(defn parse-line [s]
  (-> s (str/split #"\s") (->> (map parse-long))))

(defn mk-mapping [[d s l]]
  (let [offset (- d s)
        right  (+ s l -1)]
    {:left s
     :right right
     :offset offset}))

(defn remap [point mapping]
  (if-let [{:keys [offset]} (some #(when (<= (:left %) point (:right %)) %) mapping)]
    (-> point (+ offset))
    point))


(defn parse-mappings [s]
  (-> s (str/split #"\n")
      (->> (map parse-line)
           (map mk-mapping)
           (sort-by :left))))

(let [[seeds & mappings] (-> (slurp "in/5.txt")
                             (str/split #"\D*:")
                             (->> (remove empty?)
                                  (map str/trim)))
      seeds              (parse-line seeds)
      mappings           (map parse-mappings mappings)]

  (-> #(reduce remap % mappings)
      (map seeds)
      sort
      first
      println))

;; 1181555926
