(ns advent-of-code-2018.day-4)


(defn parse-int [s]
  (Integer. (re-find #"[0-9]*" s)))

(defn digest-input
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-4.txt"))]
    (->> (reduce
           (fn [acc line]
             (let [[year month day hour minute event] (->> (re-find #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\].*(\#\d+|falls|wakes)" line)
                                                           (rest))]
               (conj acc [(str year month day hour minute)
                          year
                          month
                          day
                          (parse-int minute)
                          event])))
           []
           (line-seq rdr))
         (sort-by first))))

(defn get-totals
  []
  (let [sorted-events (digest-input)]
    (->> (reduce
           (fn [[totals id sleep-minute] [_ year month day minute event]]
             (cond
               (= event "falls")
               [totals id minute]
               (= event "wakes")
               [(reduce
                  (fn [acc min] (-> (update-in acc [id min] #(inc (or % 0)))
                                    (update-in [id :total] #(+ (- minute sleep-minute) (or % 0)))))
                  totals
                  (range sleep-minute minute)) id nil]
               :default   ;event is an id
               [totals (parse-int (subs event 1)) nil]))
           []
           sorted-events)
         (first))))


(defn part-1
  []
  (let [by-id (->> (get-totals)
                   (sort-by (comp :total val) >))
        [sleepiest-id minutes] (first by-id)
        sleepiest-minute (first (second (sort-by val > minutes)))]
    (* sleepiest-id sleepiest-minute)))

(defn part-2
  []
  (let [totals (get-totals)
        [id minute _] (reduce
                        (fn [[cur-id cur-minute cur-times] [id schedule]]
                          (let [[new-id new-minute new-times] (reduce
                                                                (fn [[cur-id cur-minute cur-times] [minute times]]
                                                                  (if (and (> times cur-times)
                                                                           (not= :total minute))
                                                                    [id minute times]
                                                                    [cur-id cur-minute cur-times]))
                                                                [nil nil 0]
                                                                schedule)]
                            (if (> new-times cur-times)
                              [new-id new-minute new-times]
                              [cur-id cur-minute cur-times])))
                        [nil nil 0]
                        totals)]
    (* id minute)))
        
