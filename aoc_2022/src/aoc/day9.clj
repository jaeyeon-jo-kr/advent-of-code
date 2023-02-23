(ns aoc.day9 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def X 0)
(def Y 1)
(def initial [0 0])

(def initial-position 
  {:H initial
   :T initial
   :visited []})

(defn touched?
  [{:keys [H T]}]
  (= H T))

(defn tail-step
  [{[hx hy]:H 
    [tx ty]:T 
    :as status}]
  (let [x-dist (- hx tx)
        y-dist (- hy ty)
        move (fn [current dist]
               (if (zero? dist) 
                 current
                 (-> dist
                     (/ (abs dist))
                     (+ current))))]
    (-> (update-in status [:T Y] move y-dist)
        (update-in [:T X] move x-dist))))

(defn head-step
  [status direct]
  (case direct
    :D (update-in status [:H Y] dec)
    :U (update-in status [:H Y] inc)
    :L (update-in status [:H X] dec)
    :R (update-in status [:H X] inc)))

(defn parse-line
  [line]
  (let [[dir cnt] (str/split line #" ")]
    [(keyword dir) (read-string cnt)]))


(defn multi-step
  [status [direct step]]
  (->> (repeat step #(head-step % direct))
       (interpose tail-step)
       (reduce (fn [status f]
                 (f status))
               status)))

(defn tail-visited
  [input]
  (->> (str/split-lines input)
       (map parse-line)
       (reduce multi-step initial-position)))

