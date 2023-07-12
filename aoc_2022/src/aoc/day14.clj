(ns aoc.day14 
  (:require [clojure.string :as str]))

(def start [500 0])

(defn debug
  ([r]
   (debug r identity))
  ([r str-fn]
   (println "D: "(str-fn r))
   r))

(defn debug-seq
  ([r]
   (debug r))
  ([str-fn r]
   (debug r str-fn)))

(defn parse-point
  [point]
  (->> (str/split (str/trim point) #",")
       (map read-string)))

(defn parse-line
  [line]
  (->> (str/split (str/trim line) #"->")
       (map parse-point)))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn max-size
  [rock-path]
  (let [item (fn [[x-max y-max] [x y]]
            [(max x-max x) (max y-max y)]) 
        line (fn [size line] (reduce item size line))]
    (reduce line [0 0] rock-path)))
  
(defn map-size
  [rock-path]
  (let [[y-max x-max] (max-size rock-path)]
    {:x-min 0
     :y-min 0
     :x-max x-max
     :y-max y-max}))

(defn init-terrain-map
  [{:keys [x-max y-max]}]
  (->> (vec (repeat (inc x-max) "."))
       (repeat (inc y-max))
       vec))

(comment 
  (vec '(1))
  (init-terrain-map {:x-max 3 :y-max 3})
  )


(defn line-point
  [a0 a1]
  (range (min a0 a1) (inc (max a0 a1))))

(defn draw-rock
  [terrain-map rock-paths]
  (let [draw-points
        (fn [t-map [[x0 y0] [x1 y1]]]
          (debug [[x0 y0] [x1 y1]] #(str "draw :" %))
          (cond
            (= x1 x0)
            (->> (line-point y0 y1)
                 (reduce (fn [t y] (assoc-in t [y x0] "#")) t-map))
            (= y1 y0)
            (->> (line-point x0 x1)
                 (reduce (fn [t x] (assoc-in t [y0 x] "#")) t-map))))

        drow-rock-path
        (fn [t-map rock-path]
          (debug rock-path #(str "rock-path : " %))
          (->> rock-path
               ((juxt rest butlast))
               (apply map (fn [p0 p1] [p0 p1]))
               (reduce draw-points t-map)))]
    (-> (reduce drow-rock-path terrain-map rock-paths))))



(comment 
  (draw-rock 
   (init-terrain-map {:x-max 3 :y-max 3})
   [[[0 2] [0 3] [1 3]] [[2 1] [2 2]]]
   )
  )
                 
                 
                 
  
  