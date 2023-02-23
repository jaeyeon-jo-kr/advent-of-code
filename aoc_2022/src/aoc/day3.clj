(ns aoc.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- compartments
  [rucksack]
  (-> (count rucksack)
      (quot 2)
      (split-at rucksack)))

(defn common-type
  [rucksack]
  (->> (compartments rucksack)
       (map set)
       (apply set/intersection)))

(defn unit-priority
  [ch]
  (cond 
    (<= (int \a) (int ch) (int \z)) 
    (-> (int ch)
        (- (int \a))
        inc)
    (<= (int \A) (int ch) (int \Z))
    (-> (int ch)
        (- 38))))

(defn parse 
  [input]
  (str/split-lines input))

(defn total-priority
  [input]
  (->> (parse input)
       (map (comp list* common-type))
       flatten
       (map unit-priority)
       (reduce +)))

(defn group-item-type
  [rucksacks]
  (->> (map set rucksacks)
       (apply set/intersection)))

(defn total-group-item-type
  [input]
  (->> (str/split-lines input)
       (partition-all 3)
       (map (comp list* group-item-type))
       flatten
       (map unit-priority)
       (reduce +)))

