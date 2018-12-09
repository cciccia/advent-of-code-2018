(ns advent-of-code-2018.day-8
  (:require [clojure.string :as str]))

(defn level->tree-idx
  [level]
  (reduce
    (fn [acc i]
      (vec (concat acc [:nodes i])))
    []
    level))

(def empty-node
  {:nodes []
   :metadata []})

(defn build-tree
  []
  (let [data (as-> (clojure.java.io/resource "day-8.txt") $
                 (slurp $)
                 (str/split $ #" ")
                 (map read-string $))]
    (loop [tree empty-node
           cur-data (drop 2 data)
           stack {}
           level []
           nodes-to-process (first data)
           metadata-to-process (second data)]
      (cond
        (empty? cur-data)
        tree
        (> nodes-to-process 0)
        (recur (assoc-in tree (concat (level->tree-idx level) [:nodes (count (get-in tree (concat (level->tree-idx level) [:nodes])))]) empty-node)
               (drop 2 cur-data)
               (assoc stack level [nodes-to-process metadata-to-process])
               (conj level (count (get-in tree (concat (level->tree-idx level) [:nodes]))))
               (first cur-data)
               (second cur-data))
        :default
        (recur (assoc-in tree (concat (level->tree-idx level) [:metadata]) (take metadata-to-process cur-data))
               (drop metadata-to-process cur-data)
               (dissoc stack level)
               (vec (drop-last level))
               (dec (first (get stack (drop-last level))))
               (second (get stack (drop-last level))))))))

(defn get-sum
  [tree]
  (apply +
         (concat [(apply + (:metadata tree))]
                 (map get-sum (:nodes tree)))))

(defn part-1
  []
  (let [tree (build-tree)]
    (get-sum tree)))

(defn get-node-val
  [node]
  (if (empty? (:nodes node))
    (apply + (:metadata node))
    (apply + (map (fn [idx]
                    (try
                      (let [child (nth (:nodes node) (dec idx))]
                        (get-node-val child))
                      (catch Exception e
                        0)))
                  (:metadata node)))))

(defn part-2
  []
  (let [tree (build-tree)]
    (get-node-val tree)))







