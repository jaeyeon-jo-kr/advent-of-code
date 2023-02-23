(ns aoc.day4
  (:require [clojure.string :as str]))

(defn parse-pairs
  [line]
  (->> (str/split line #",")
       (map (comp 
             #(map read-string %)
             #(str/split % #"-")))))

(defn fully-contained?
  [[[a b] [c d]]]
  (or (<= a c d b)
      (<= c a b d)))

(defn contain-count
  [input]
  (->> (str/split-lines input)
       (map (comp fully-contained? parse-pairs))
       (filter identity)
       count))

(defn overlap?
  [[[a b] [c d]]]
  (or (<= a c b)
      (<= a d b)
      (<= c a d)
      (<= c b d)))

(defn overlap-count
  [input]
  (->> (str/split-lines input)
       (map parse-pairs)
       (filter overlap?)
       count))

(comment 
  (->> (slurp "./day4_input.txt")
       str/split-lines
      (map parse-pairs)
       (filter overlap?))
  )