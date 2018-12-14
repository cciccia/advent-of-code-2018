(ns advent-of-code-2018.day-13
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn build-track
  []
  (let [lines (->> (-> (clojure.java.io/resource "day-13.txt")
                       (slurp)
                       (str/split #"\n"))
                   (map #(str/split % #"")))]
    (reduce
      (fn [[track carts] [y line]]
        (let [[new-track new-carts] (reduce
                                      (fn [[track carts] [x c]]
                                        (case c
                                          "^"
                                          [(assoc track [x y] "|")
                                           (assoc carts [x y] [:up :left])]
                                          "v"
                                          [(assoc track [x y] "|")
                                           (assoc carts [x y] [:down :left])]
                                          "<"
                                          [(assoc track [x y] "-")
                                           (assoc carts [x y] [:left :left])]
                                          ">"
                                          [(assoc track [x y] "-")
                                           (assoc carts [x y] [:right :left])]
                                          [(assoc track [x y] c) carts]))
                                      [track carts]
                                      (map-indexed vector line))]
          [new-track
           new-carts]))
      [{} {}]
      (map-indexed vector lines))))

(defn toggle-command
  [command]
  (case command
    :left :straight
    :straight :right
    :right :left))

(defn turn-left
  [direction]
  (case direction
    :left :down
    :down :right
    :right :up
    :up :left))

(defn turn-right
  [direction]
  (case direction
    :left :up
    :up :right
    :right :down
    :down :left))

(defn handle-intersection
  [direction command]
  (case command
    :left
    (turn-left direction)
    :right
    (turn-right direction)
    direction))

(defn move
  [track [[x y] [direction command]]]
  (let [tile (get track [x y])]
    (if (= tile "+")
      (let [new-direction (handle-intersection direction command)
            new-command (toggle-command command)]
        (case new-direction
          :left [[(dec x) y] [:left new-command]]
          :up [[x (dec y)] [:up new-command]]
          :right [[(inc x) y] [:right new-command]]
          :down [[x (inc y)] [:down new-command]]))
      (case direction
        :left
        (case tile
          "-"
          [[(dec x) y] [:left command]]
          "/"
          [[x (inc y)] [:down command]]
          "\\"
          [[x (dec y)] [:up command]])
        :up
        (case tile
          "|"
          [[x (dec y)] [:up command]]
          "/"
          [[(inc x) y] [:right command]]
          "\\"
          [[(dec x) y] [:left command]])
        :right
        (case tile
          "-"
          [[(inc x) y] [:right command]]
          "/"
          [[x (dec y)] [:up command]]
          "\\"
          [[x (inc y)] [:down command]])
        :down
        (case tile
          "|"
          [[x (inc y)] [:down command]]
          "/"
          [[(dec x) y] [:left command]]
          "\\"
          [[(inc x) y] [:right command]])))))

(defn is-after?
  [[ref-x ref-y] [x y]]
  (if (> y ref-y)
    true
    (> x ref-x)))

(defn next-cart
  [[ref-x ref-y] carts-at-rest ordered-carts]
  (some (fn [[[x y] cart]]
          (when (and (not (contains? carts-at-rest [x y]))
                     (is-after? [ref-x ref-y] [x y]))
            [[x y] cart]))
        ordered-carts))

(defn order-carts
  [carts]
  (sort-by (fn [[[x y] _]] (+ (* 1000 y) x)) carts))

(defn part-1
  []
  (let [[track initial-carts] (build-track)]
    (loop [carts initial-carts
           carts-at-rest #{}
           current-loc [-1 -1]]
      (let [ordered-carts (order-carts carts)
            next-cart-to-move (next-cart current-loc carts-at-rest ordered-carts)]
        (if (nil? next-cart-to-move)
          (recur carts
                 #{}
                 [-1 -1])
          (let [moved-cart (move track next-cart-to-move)]
            (if (contains? carts (first moved-cart))
              (first moved-cart)
              (recur (-> carts
                         (dissoc (first next-cart-to-move))
                         (assoc (first moved-cart) (second moved-cart)))
                     (conj carts-at-rest (first moved-cart))
                     (first next-cart-to-move)))))))))

(defn part-2
  []
  (let [[track initial-carts] (build-track)]
    (loop [carts initial-carts
           carts-at-rest #{}
           current-loc [-1 -1]]
      (let [ordered-carts (order-carts carts)
            next-cart-to-move (next-cart current-loc carts-at-rest ordered-carts)
            moved-cart (when next-cart-to-move (move track next-cart-to-move))]
        (cond
          (and (nil? next-cart-to-move)
               (= 1 (count carts)))
          (first (first carts))
          (nil? next-cart-to-move)
          (recur carts
                 #{}
                 [-1 -1])
          (contains? carts (first moved-cart))
          (recur (-> carts
                     (dissoc (first moved-cart))
                     (dissoc (first next-cart-to-move)))
                 carts-at-rest
                 (first next-cart-to-move))
          :default
          (recur (-> carts
                     (dissoc (first next-cart-to-move))
                     (assoc (first moved-cart) (second moved-cart)))
                 (conj carts-at-rest (first moved-cart))
                 (first next-cart-to-move)))))))