(ns aoc.day1
  (:require [hickory.core :as hi]
            [clojure.walk :as w]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn read-problem
  []
  (slurp "https://adventofcode.com/2022/day/1"))

(def example-calories
  (->> (-> "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n"
           (str/split #"\n\n"))
       (map #(->> (str/split % #"\n")
                  (map read-string)))))

(defn parse 
  [input]
  (->> (-> input
           (str/split #"\n\n"))
       (map #(->> (str/split % #"\n")
                  (map read-string)))))

(defn ->datum
  [text]
  (->> (str/split text #"\n\n")
       (map #(->> (str/split % #"\n")
                  (map read-string)))))

(defn find-total-calories
  [calories]
  (->> (map (partial reduce +) calories)
       (apply max)))

(defn find-three-calories
  [calories]
  (->> (map (partial reduce +) calories)
       (sort-by identity >)
       (take 3)
       (reduce +)))

(comment
  (-> (slurp "day1input.txt")
      ->datum
      find-total-calories)
  (-> (slurp "day1input.txt")
      ->datum
      find-three-calories)

  (-> example-calories
      find-three-calories)
  )


(comment
  (->>   example-calories
         (map (partial reduce +))
         (apply max)

         ))



(comment
  (-> (->> (-> (read-problem)
               hi/parse
               hi/as-hiccup)
           (drop 2)
           first
           (filter coll?)
           pp/pprint
           )
      )

  hi/parse
  )
