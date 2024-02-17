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
    {:left   s
     :right  right
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


;; 2
;;

(defn parse-seeds-ranges [seeds]
  (->> seeds
       parse-line
       (partition 2)
       (sort-by first)
       (map (fn [[left length]]
              {:left  left
               :right (dec (+ left length))}))))

(defn intersect [a b]
  (cond
    ;; a in b
    (and
      (<= (:left b) (:left a))
      (<= (:right a) (:right b)))

    {:intersection a
     :rem          []}

    ;; b in a
    (and
      (<= (:left a) (:left b))
      (<= (:right b) (:right a)))

    {:intersection (dissoc b :offset)
     :rem          [(assoc a :right (dec (:left b))) (assoc a :left (inc (:right b)))]}

    ;; left side of a is in b
    (and
      (<= (:left b) (:left a) (:right b))
      (> (:right a) (:right b)))

    {:intersection
     {:left  (:left a)
      :right (:right b)}
     :rem
     [(assoc a :left (inc (:right b)))]}

    ;;right side of a is in b
    (and
      (< (:left a) (:left b))
      (<= (:left b) (:right a) (:right b)))

    {:rem
     [(assoc a :right (dec (:left b)))]
     :intersection {:left  (:left b)
                    :right (:right a)}}
    :else
    {:rem          [a]
     :intersection nil}))

(defn shift [range offset]
  (some-> range
    (update :left + offset)
    (update :right + offset)))

(defn remap-ranges [ss ms]
  (loop [maps       ms
         to-process ss
         result     []]
    (if-let [m (first maps)]
      (let [maps       (rest maps)
            mapped     (map #(intersect % m) to-process)
            shifted    (map #(-> % :intersection (shift (:offset m))) mapped)
            result     (apply conj result (remove nil? shifted))
            to-process (for [{:keys [rem]} mapped r rem] r)
            #_         (println "result" result)
            #_         (println "to process" to-process)
            ]
        (recur maps to-process result))
      (concat result to-process))))

(let [[seeds & mappings] (-> (slurp "in/5.txt")
                           (str/split #"\D*:")
                           (->> (remove empty?)
                             (map str/trim)))
      seeds              (parse-seeds-ranges seeds)
      mappings           (map parse-mappings mappings)
      rds                (reduce remap-ranges seeds mappings)]
  (->> rds
    (map :left)
    sort
    first
    println))

;; 37806486
