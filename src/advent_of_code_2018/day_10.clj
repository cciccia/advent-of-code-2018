(ns advent-of-code-2018.day-10)

(defn get-balls
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-10.txt"))]
    (reduce
      (fn [acc line]
        (conj acc (->> (re-find #"position=< ?(-?\d+),  ?(-?\d+)> velocity=< ?(-?\d+),  ?(-?\d+)>" line)
                       (rest)
                       (mapv read-string))))
      []
      (line-seq rdr))))


(defn part-1
  "started at 10700. this was derived by looking at the relationship between velocity and position for the first two points.
   They converged around the 10700 mark (turns out it was 10710) and from there it was easy to step through.
   Another way to do it would be to look where the value of (min (max Yvalue - minYvalue))
   Or some graph bullshit with connected tiles"
  [start-t]
  (loop [t start-t]
    (let [balls (get-balls)
          occupied (reduce
                     (fn [acc [pos-x pos-y vel-x vel-y]]
                         (conj acc [(+ pos-x (* vel-x t))
                                    (+ pos-y (* vel-y t))]))
                     #{}
                     balls)
          occupied-x (sort-by first occupied)
          occupied-y (sort-by second occupied)]
      (doseq [y (range (second (first occupied-y)) (inc (second (last occupied-y))))
              x (range (first (first occupied-x)) (inc (first (last occupied-x))))]
        (print (if (contains? occupied [x y])
                 "#"
                 "."))
        (if (= x (first (last occupied-x)))
          (print "\n")))
      (println t)
      (Thread/sleep 100)
      (recur (inc t)))))


