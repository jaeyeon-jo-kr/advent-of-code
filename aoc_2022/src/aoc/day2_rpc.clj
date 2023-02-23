(ns aoc.day2-rpc
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def point {:X 1 :Y 2 :Z 3})
(def outcome {:lost 0 :draw 3 :won 6})
(def result-map
  {:A {:Z :lost
       :X :draw
       :Y :won}
   :B {:Z :won
       :X :lost
       :Y :draw}
   :C {:Y :lost
       :X :won
       :Z :draw}})
(defn result
  [opponent your]
  (get-in result-map [opponent your]))

(def ->item keyword)

(defn round
  [opponent your]
  (+ (outcome (result opponent your))
     (point your)))

(defn total-score
  [strategy]
  (transduce (map (partial apply round)) + strategy))

(defn parse
  [text]
  (->> text
       (clojure.string/split-lines)
       (map #(->> (clojure.string/split % #" ")
                  (map ->item)))))
(defn solve-part1
  [input-file]
  (-> (slurp input-file)
      parse
      total-score))


(def ->goal
  {:X :lost :Y :draw :Z :won})

(defn round-point2
  [opponent yours]
  (let [goal (->goal yours)]
    (-> (get result-map opponent)
        set/map-invert
        goal
        point
        (+ (outcome goal)))))

(defn total-score2
  [strategy]
  (transduce (map (partial apply round-point2)) + strategy))



(defn solve-part2
  [input-file]
  (-> (slurp input-file)
      parse
      total-score2))