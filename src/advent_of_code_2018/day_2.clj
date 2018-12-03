(ns advent-of-code-2018.day-2
  (:require [clojure.string :as str]
            [clojure.data :as data]))

(defn part-1
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-2.txt"))]
    (let [{:keys [two three]} (reduce
                                (fn [{:keys [two three]} line]
                                  (let [freqs (frequencies line)
                                        two? (some (fn [[_ v]] (= 2 v)) freqs)
                                        three? (some (fn [[_ v]] (= 3 v)) freqs)]
                                    {:two   (cond-> two two? inc)
                                     :three (cond-> three three? inc)}))
                                {:two 0 :three 0}
                                (line-seq rdr))]
      (* two three))))

(defn part-2
  []
  (let [data (slurp (clojure.java.io/resource "day-2.txt"))
        lines (str/split-lines data)
        num-lines (count lines)]
    (loop [i 0 j 1]
      (let [i-vec (str/split (nth lines i) #"")
            j-vec (str/split (nth lines j) #"")
            [_ _ same-or-nil] (data/diff i-vec j-vec)
            same (remove nil? same-or-nil)]
        (if (= (count same) (dec (count i-vec)))
          (str/join same)
          (if (= j (dec num-lines))
            (recur (inc i) (+ i 2))
            (recur i (inc j))))))))

      


