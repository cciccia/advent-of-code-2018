(ns advent-of-code-2018.day-1)

(defn part-1
  []
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-1.txt"))]
    (reduce
      #(+ %1 (read-string %2))
      0
      (line-seq rdr))))

(defn loopdoop
  [acc freq]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "day-1.txt"))]
    (let [result (reduce
                   (fn [[acc freq done?] line]
                     (if done?
                       [acc freq done?]
                       (let [new-freq (+ freq (read-string line))]
                         (if (contains? acc new-freq)
                           [acc new-freq true]
                           [(conj acc new-freq) new-freq false]))))
                   [acc freq false]
                   (line-seq rdr))]
      (if (last result)
        (second result)
        (loopdoop (first result) (second result))))))

(defn part-2
  []
  (loopdoop #{0} 0))