(ns advent-of-code-2018.day-6)

(defn distance
  [[xa ya] [xb yb]]
  (+ (Math/abs (- xa xb))
     (Math/abs (- ya yb))))

(defn move
  [[cx cy] dir]
  (case dir
    :right [(inc cx) cy]
    :up [cx (inc cy)]
    :left [(dec cx) cy]
    :down [cx (dec cy)]))

(defn rotate
  [dir]
  (case dir
    :right :up
    :up :left
    :left :down
    :down :right))

(defn get-points
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-6.txt"))]
    (reduce
      (fn [acc line]
        (let [[x y] (->> (re-find #"(\d+), (\d+)" line)
                         (rest)
                         (map read-string))]
          (conj acc [(count acc) x y])))
      []
      (line-seq rdr))))

(defn closest-id-to
  [points [xb yb]]
  (let [distances (->> points
                       (pmap (fn [[id xa ya]]
                               [id (distance [xa ya] [xb yb])]))
                       (sort-by second))]
    (when (not= (second (first distances))
                (second (second distances)))
      (first (first distances)))))

(defn inc-closest
  [distances id]
  (if id
    (update distances id #(inc (or % 0)))
    distances))

(defn part-1
  []
  (let [points (get-points)
        points-by-x (sort (map second points))
        points-by-y (sort (map #(nth % 2) points))
        meanx (int (Math/floor (/ (- (last points-by-x) (first points-by-x)) 2)))
        meany (int (Math/floor (/ (- (last points-by-y) (first points-by-y)) 2)))]
    (loop [distances {} [x y] [meanx meany] inc? false i 1 j 0 dir :right]
      (let [id (closest-id-to points [x y])]
        (if (and (= 0 (mod i 50))
                 (= j 0))
          (println (sort-by val distances)))  ;;will run forever but easy to eyeball the correct answer quickly
        (if (= (dec i) j)
          (recur (inc-closest distances id) (move [x y] dir) (not inc?) (if inc? (inc i) i) 0 (rotate dir))
          (recur (inc-closest distances id) (move [x y] dir) inc? i (inc j) dir))))))

(defn total-distance-lt-10000?
  [points [xb yb]]
  (let [distance (->> points
                      (pmap
                        (fn [[_ xa ya]] (distance [xa ya] [xb yb])))
                      (reduce + 0))]
    (< distance 10000)))

(defn part-2
  []
  (let [points (get-points)
        points-by-x (sort (map second points))
        points-by-y (sort (map #(nth % 2) points))
        meanx (int (Math/floor (/ (- (last points-by-x) (first points-by-x)) 2)))
        meany (int (Math/floor (/ (- (last points-by-y) (first points-by-y)) 2)))]
    (loop [total 0 [x y] [meanx meany] inc? false i 1 j 0 dir :right]
      (let [region? (total-distance-lt-10000? points [x y])]
        (if (and (= 0 (mod i 50))
                 (= j 0))
         (println total))  ;;will run forever but easy to eyeball the correct answer quickly
        (if (= (dec i) j)
          (recur (cond-> total region? inc) (move [x y] dir) (not inc?) (if inc? (inc i) i) 0 (rotate dir))
          (recur (cond-> total region? inc) (move [x y] dir) inc? i (inc j) dir))))))









