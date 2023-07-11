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

(comment
  (solve-part1 (slurp "./day13_input.txt"))
  (sequential? '())
  (next [1 2 3])
  (list* [1 2 3])
  (compare-list '(1,1,3,1,1)
                '(1,1,5,1,1))

  (def example "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")
  
  (->> (str/split example #"\n\n")
       (map (comp #(apply compare-list %)
                  #(map read-string %)
             str/split-lines)))
  (compare-list [[[]]]
                [[]])
(compare-list [[1],[2,3,4]]
              [[1],4])
  (compare-list [[4,4],4,4]
                [[4,4],4,4,4])

  (read-string "[[1],[2,3,4]]")
  (take-while (complement #(= % \])) "[1,2,3],3")

  )