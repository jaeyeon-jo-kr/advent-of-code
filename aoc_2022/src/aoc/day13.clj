(ns aoc.day13
  (:require [clojure.string :as str]))

(defn compare-list
  [left right]
  (println "compare " left " and " right) 
  
  (cond
    (and (integer? left) (integer? right))
    (cond (= left right) :same
          (< left right) :left
          (< right left) :right)

    (and left (nil? right))
    (do (println "- Right side ran out of items, so inputs are not in the right order")
        :right)

    (and (nil? left) right)
    (do (println "- Left side ran out of items, so inputs are in the right order")
        :left)

    (and (integer? left) (sequential? right))
    (compare-list (list left) right)

    (and (sequential? left) (integer? right))
    (compare-list left (list right))

    (and (sequential? left) (sequential? right))
    (let [result (compare-list (first left) (first right))]
      (if (= :same result)
        (compare-list (next left) (next right))
        result))
    :else :same))

(defn solve-part1
  [example]
  (->> (str/split example #"\n\n")
       (map (comp #(apply compare-list %)
                  #(map read-string %)
                  str/split-lines))
       (keep-indexed (fn [i r]
                       (when (= :left r)
                         (inc i))))
       (reduce +)))

(defn solve-part2
  [example]
  (->> (str/split example #"\n\n")
       (map (comp #(map read-string %)
                  str/split-lines))
       (mapcat (juxt first second))
       (cons [[2]])
       (cons [[6]])
       (sort (comp {:left -1
                    :same 0
                    :right 1} compare-list))
       (keep-indexed (fn [i coll]
                       (when (#{[[2]] [[6]]} coll)
                         (inc i))))
       (reduce *)))
