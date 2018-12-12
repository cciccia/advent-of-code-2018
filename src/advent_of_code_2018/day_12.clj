(ns advent-of-code-2018.day-12
  (:require [clojure.string :as str]))

(defn position->repr
  [state pos]
  (->> (range (- pos 2) (+ pos 3))
       (map (fn [i] (if (contains? state i) "#" ".")))
       (str/join)))


(defn get-state-and-rules
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-12.txt"))]
    (let [initial-state (as-> (first (line-seq rdr)) $
                              (re-find #"initial state: ([.#]+)" $)
                              (second $))
          rules (reduce
                  (fn [acc line]
                    (let [[prev next] (->> (re-find #"([.#]{5}) => ([.#])" line)
                                           (rest))]
                      (assoc acc prev next)))
                  {}
                  (drop 1 (line-seq rdr)))]
      [initial-state rules])))

(defn sum-state
  [cur-state cur-min]
  (reduce (fn [acc [i chr]]
            (+ acc (if (= \# chr)
                     (+ cur-min i)
                     0)))
          0
          (map-indexed vector cur-state)))


(defn part-1
  [end]
  (let [[initial-state rules] (get-state-and-rules)]
    (loop [states-by-state {initial-state 0}
           states-by-time {0 [0 initial-state]}
           t 0]
      (let [[cur-min cur-state] (get states-by-time t)]
        (if (= end t)
          (sum-state cur-state cur-min)
          (let [cur-state (str "...." cur-state "....")
                new-state (reduce
                            (fn [new-state pos]
                              (str new-state (get rules (subs (str ".." cur-state "..") pos (+ pos 5)))))
                            ""
                            (range 2 (- (.length cur-state) 2)))
                dots #(= \. %)
                new-min (+ cur-min -2 (count (take-while dots new-state)))
                new-state (->> (drop-while dots new-state)
                               (str/join)
                               (str/reverse)
                               (drop-while dots)
                               (str/join)
                               (str/reverse))]
            (if-let [cycle (get states-by-state new-state)]
              (let [cycle-start cycle
                    cycle-length (- (inc t) cycle-start)
                    cycle-time (+ cycle-start (mod (- end cycle-start) cycle-length))
                    [cycle-min cycle-state] (get states-by-time cycle-time)]
                (sum-state cycle-state (+ cycle-min (long (Math/floor (/ (- end cycle-start) cycle-length))))))
              (recur (assoc states-by-state new-state (inc t))
                     (assoc states-by-time (inc t) [new-min new-state])
                     (inc t)))))))))


