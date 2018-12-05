(ns advent-of-code-2018.day-5
  (:require [clojure.string :as str]))

(defn str->chr
  [s]
  (char (first (.getBytes s))))

(defn reactive?
  [a b]
  (and (= (str/lower-case a) (str/lower-case b))
       (or (and (Character/isUpperCase (str->chr a))
                (Character/isLowerCase (str->chr b)))
           (and (Character/isUpperCase (str->chr b))
                (Character/isLowerCase (str->chr a))))))

(defn react
  [data-vec]
  (loop [cur-data-vec data-vec i 0]
    (let [cur-chr (nth cur-data-vec i)
          next-chr (nth cur-data-vec (max (min (dec (count cur-data-vec)) (inc i)) 1))]
      (cond
        (= i (dec (count cur-data-vec)))
        (count cur-data-vec)
        (reactive? cur-chr next-chr)
        (recur (vec (concat (subvec cur-data-vec 0 i) (if (>= (+ i 2) (count cur-data-vec))
                                                        []
                                                        (subvec cur-data-vec (+ i 2)))))
               (max 0 (dec i)))
        :default
        (recur cur-data-vec (inc i))))))

(defn part-1
  []
  (let [data (slurp (clojure.java.io/resource "day-5.txt"))
        data-vec (str/split data #"")]
    (react data-vec)))

(defn part-2
  []
  (let [data (slurp (clojure.java.io/resource "day-5.txt"))
        data-vec (str/split data #"")
        data-set (set (map str/lower-case data-vec))]
    (->> data-set
         (pmap (fn [chr]
                 (let [sans-chr (->> data-vec
                                     (remove #(or (= (str/lower-case chr) %)
                                                  (= (str/upper-case chr) %)))
                                     (vec))]
                   (react sans-chr))))
         (apply min))))







