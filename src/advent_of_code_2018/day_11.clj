(ns advent-of-code-2018.day-11)

(defn cell->level
  [input [x y]]
  (let [rack-id (+ x 10)
        start-level (* rack-id y)
        int-1 (+ input start-level)
        int-2 (* rack-id int-1)
        int-3 (mod (int (Math/floor (/ int-2 100))) 10)]
    (- int-3 5)))

(defn sum-cells
  [cache input size [start-x start-y]]
  (let [new-result (+ (get cache [start-x start-y (dec size)])
                      (reduce
                        (fn [cur-result offset-y]
                          (+ cur-result (get cache [(+ start-x (dec size)) (+ start-y offset-y) 1] 0)))
                        0
                        (range 0 (dec size)))
                      (reduce
                        (fn [cur-result offset-x]
                          (+ cur-result (get cache [(+ start-x offset-x) (+ start-y (dec size)) 1] 0)))
                        0
                        (range 0 size)))]
    [(assoc cache [start-x start-y size] new-result) new-result]))

(defn build-one-cache
  [input]
  (reduce
    (fn [cache y]
      (reduce
        (fn [cache x]
          (assoc cache [x y 1] (cell->level input [x y])))
        cache
        (range 1 301)))
    {}
    (range 1 301)))

(defn part-1
  "I'm just going to settle for 10 minutes while my computer is unusable being good enough"
  [input size]
  (let [cache (build-one-cache input)]
    (->> (pmap
           (fn [y]
             (->> (pmap
                    (fn [x]
                      (reduce
                        (fn [[cache cell size max] cur-size]
                          (let [[new-cache new-max] (sum-cells cache input cur-size [x y])]
                            (if (> new-max max)
                              [new-cache [x y] cur-size new-max]
                              [new-cache cell size max])))
                        [cache [] 0 -999]
                        (range 2 (inc size))))
                    (range 1 301))
                  (sort-by last >)
                  (first)))
           (range 1 301))
         (sort-by last >)
         (first)
         (drop 1))))