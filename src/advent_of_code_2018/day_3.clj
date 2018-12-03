(ns advent-of-code-2018.day-3
  (:require [clojure.set :as set]
            [clojure.data :as data]))

(defn get-claims
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-3.txt"))]
    (reduce
      (fn [acc line]
        (let [[id left top width height] (->> (re-find #"\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)
                                              (rest)
                                              (map read-string))]
          (reduce
            (fn [a k] (update a k #(conj (or % #{}) id)))
            acc
            (for [x (range left (+ left width))
                  y (range top (+ top height))]
              [x y]))))
      {}
      (line-seq rdr))))

(defn part-1
  []
  (let [claims (get-claims)]
    (->> claims
         (filter (fn [[_ v]] (> (count v) 1)))
         (count))))

(defn part-2
  []
  (let [claims (get-claims)
        claims-with-dupes (filter (fn [[_ v]] (> (count v) 1)) claims)
        ids-with-dupes (reduce
                         (fn [acc [_ v]]
                           (set/union acc (set v)))
                         #{}
                         claims-with-dupes)]
    (set/difference (set (range 1 1306))
                    ids-with-dupes)))


