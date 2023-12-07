(ns day1
  (:require [clojure.string :as string]))

(def problem (slurp "input.txt"))


(defn first-digit
  [line]
  (-> (filter #(Character/isDigit %) line)
      first
      int
      (- (int \0))))

(defn last-digit
  [line]
  (->> line
       reverse
       first-digit))

(defn solve-part1
  []
  (->> (string/split-lines problem)
       (map (fn [line]
              (+ (last-digit line)
                 (* 10
                    (first-digit line)))))
       (reduce +)))

(def str->int
  {"1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9
   "one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn calibration
  [line]
  (let [coll
        (re-seq
         #"[1-9]|one|two|three|four|five|six|seven|eight|nine" line)]
    (+ (* 10 (str->int (first coll)))
       (str->int (last coll)))))

(defn solve-part2
  []
  (->> (string/split-lines problem)
       (map calibration)
       (reduce +)))

(comment
  (solve-part1)
  (solve-part2))

