(ns advent-of-code-2018.day-9
  (:require [clojure.zip :as zip]))

; naive vector impl
; into is O(N) when combining two vectors which makes this run O(n^2)
; still takes around 60? seconds for part 1. Part 2, forget about it

(defn insert-at
  [board marble i]
  (-> (into (subvec board 0 (inc i)) [marble])
      (into (subvec board (inc i)))))

(defn pop-at
  [board i]
  [(into (subvec board 0 i) (subvec board (inc i))) (nth board i)])

(defn move
  [board i spaces counter?]
  (-> ((if counter?
         -
         +) i spaces)
      (mod (count board))))

(defn next-turn
  [players player]
  (-> (mod player players)
      (inc)))

(defn part-1
  [players marbles]
  (loop [scores (reduce
                  #(assoc %1 %2 0)
                  {}
                  (range 1 (inc players)))
         board [0]
         active 0
         marble 1
         player 1]
    (cond
      (> marble marbles)
      (->> scores
           (sort-by val >)
           (first)
           (val))
      (= 0 (mod marble 23))
      (let [pop-loc (move board active 7 true)
            [new-board popped-marble] (pop-at board (move board active 7 true))]
        (recur (update scores player + marble popped-marble)
               new-board
               (move new-board pop-loc 0 false)
               (inc marble)
               (next-turn players player)))
      :default
      (let [insert-after (move board active 1 false)
            new-board (insert-at board marble insert-after)]
        (recur scores
               new-board
               (inc insert-after)
               (inc marble)
               (next-turn players player))))))


;; faster clojure.zip implementation
;; runs at O(n) which is acceptable, comes back instantaneously for part 1, reasonably quickly for part 2
;; easier to reason about too

(defn move-right
  [loc amount]
  (loop [cur-loc loc
         i amount]
    (if (= i 0)
      cur-loc
      (let [new-loc (or (zip/right cur-loc) (zip/leftmost cur-loc))]
        (recur new-loc (dec i))))))

(defn move-left
  [loc amount]
  (loop [cur-loc loc
         i amount]
    (if (= i 0)
      cur-loc
      (let [new-loc (or (zip/left cur-loc) (zip/rightmost cur-loc))]
        (recur new-loc (dec i))))))

(defn zip-add-marble
  [loc marble]
  (-> loc
      (move-right 1)
      (zip/insert-right marble)
      (move-right 1)))

(defn zip-remove-marble
  [loc]
  (let [remove-loc (move-left loc 7)]
    [(-> (zip/remove remove-loc)
         (move-right 1))
     (zip/node remove-loc)]))


(defn part-2
  [players marbles]
  (loop [scores (reduce
                  #(assoc %1 %2 0)
                  {}
                  (range 1 (inc players)))
         board-loc (zip/leftmost (zip/down (zip/seq-zip (seq [0]))))
         marble 1
         player 1]
    (cond
      (> marble marbles)
      (->> scores
           (sort-by val >)
           (first)
           (val))
      (= 0 (mod marble 23))
      (let [[new-board-loc popped-marble] (zip-remove-marble board-loc)]
        (recur (update scores player + marble popped-marble)
               new-board-loc
               (inc marble)
               (next-turn players player)))
      :default
      (let [new-board-loc (zip-add-marble board-loc marble)]
        (recur scores
               new-board-loc
               (inc marble)
               (next-turn players player))))))


