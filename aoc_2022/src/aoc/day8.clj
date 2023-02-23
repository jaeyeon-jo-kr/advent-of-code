(ns aoc.day8
  (:require [clojure.string :as str]))

(defn parse-map
  [text]
  (->> (str/split-lines text)
       (mapv 
        (partial mapv #(- (int %) (int \0))))))

(def tree-map
  [[3 0 3 7 3]
   [2 5 5 1 2]
   [6 5 3 3 2]
   [3 3 5 4 9]
   [3 5 3 9 0]])

(defn up-coll
  [tree-map row col]
  (->> tree-map
       (take row)
       (map #(nth % col))
       reverse))

(defn down-coll
  [tree-map row col]
  (->> tree-map
       (drop (inc row))
       (map #(nth % col))))

(defn left-coll
  [tree-map row col]
  (->> (get tree-map row)
       (take col)
       reverse))

(defn right-coll
  [tree-map row col]
  (->> (get tree-map row)
       (drop (inc col))))

(defn visible-from?
  [tree coll]
  (every? #(< % tree) coll))

(defn visible?
  [tree-map row col]
  (some (fn [f]
          (visible-from?
           (get-in tree-map [row col])
           (f tree-map row col)))
        [up-coll down-coll left-coll right-coll]))

(defn visible-count
  [tree-map]
  (let [row-indexes (range (count tree-map))
        col-indexes (range (count (first tree-map)))]
    (->> (for [row row-indexes
               col col-indexes]
           (visible? tree-map row col))
         (keep identity)
         count)))

(defn tree-view-count
  [value coll]
  (let [small-tree (->> (take-while #(< % value) coll)
                count)]
    (if (= small-tree (count coll))
      small-tree
      (inc small-tree))))

(defn scenic-score
  [tree-map row col]
  (let [tree-height (get-in tree-map [row col])]
    (->> [up-coll down-coll left-coll right-coll]
         (map (fn [f]
                (->> (f tree-map row col)
                     (tree-view-count tree-height))))
         (reduce *))))

(defn max-scening-score
  [tree-map]
  (let [row-indexes (range (count tree-map))
        col-indexes (range (count (first tree-map)))]
    (->> (for [row row-indexes
               col col-indexes]
           (scenic-score tree-map row col))
         (reduce max))))

(comment 
 (max-scening-score tree-map)
  (scenic-score tree-map 1 2)
  (->> (take-while #(< % 5) '(5 2))
       count))
  
  