(ns advent-of-code-2018.day-7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def all-steps #{"A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"})

(defn get-steps
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-7.txt"))]
    (reduce
      (fn [acc line]
        (let [[prereq next] (->> (re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." line)
                                 (rest))]
          (update acc next #(conj (or % #{}) prereq))))
      {}
      (line-seq rdr))))

(defn comes-befores?
  [a b]
  (< (compare a b) 0))

(defn get-runnable
  [steps done-steps]
  (-> (reduce
        (fn [acc [next prereq]]
          (if-not (set/subset? prereq done-steps)
            (disj acc next)
            acc))
        all-steps
        steps)
      (set/difference done-steps)))

(defn find-earliest-with-requirement
  [steps done-steps]
  (-> (get-runnable steps done-steps)
      (sort)
      (first)))

(defn get-ordered-steps
  []
  (let [steps (get-steps)]
    (loop [todo []]
      (if (= 26 (count todo))
        todo
        (let [next (find-earliest-with-requirement steps (set todo))]
          (recur (conj todo next)))))))

(defn part-1
  []
  (str/join (get-ordered-steps)))

(defn job->length
  [job]
  (- (first (.getBytes job)) 4))

(defn distribute-jobs
  [workers jobs time]
  (loop [cur-workers workers
         cur-jobs jobs]
    (let [i (some (fn [[i worker]] (when (nil? worker)
                                     i))
                  (map-indexed #(vector %1 %2) cur-workers))]
      (if (or (nil? i)
              (empty? cur-jobs))
        cur-workers
        (recur (assoc cur-workers i [(first cur-jobs) (+ time (job->length (first cur-jobs)))]) (rest cur-jobs))))))

(defn clear-jobs
  [workers done-jobs time]
  (reduce
    (fn [[cur-workers cur-done-jobs] i]
      (let [cur-worker (nth cur-workers i)]
        (if (= time (second cur-worker))
          [(assoc cur-workers i nil) (conj cur-done-jobs (first cur-worker))]
          [cur-workers cur-done-jobs])))
    [workers done-jobs]
    (range 0 5)))

(defn part-2
  []
  (let [steps (get-steps)]
    (loop [workers [nil nil nil nil nil]
           done-jobs #{}
           time 0]
      (let [[workers done-jobs] (clear-jobs workers done-jobs time)]
        (if (= 26 (count done-jobs))
          time
          (let [jobs (sort (set/difference (get-runnable steps done-jobs) (->> (map first workers)
                                                                               (remove nil?)
                                                                               (set))))
                new-workers (distribute-jobs workers jobs time)]
            (recur new-workers done-jobs (inc time))))))))



