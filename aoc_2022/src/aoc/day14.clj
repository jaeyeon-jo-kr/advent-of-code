(ns aoc.day14
  (:require [clojure.string :as str]))

(def start [500 0])

(def example "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn debug
  ([r]
   (debug r identity))
  ([r str-fn]
   (println "D: " (str-fn r))
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

(comment
  (-> (slurp "./day14_input.txt")
      parse-input)

  (parse-input example))

(defn max-size
  [rock-path]
  (let [item (fn [[x-max y-max] [x y]]
               [(max x-max x) (max y-max y)])
        line (fn [size line] (reduce item size line))]
    (reduce line [0 0] rock-path)))

(defn map-size
  [rock-path]
  (let [[x-max y-max] (max-size rock-path)]
    {:x-min 0
     :y-min 0
     :x-max x-max
     :y-max y-max}))

(comment
  (def rock-path (parse-input example))
  (map-size rock-path))

(defn init-terrain-map
  [x-max y-max]
  (->> (vec (repeat (inc x-max) "."))
       (repeat (inc y-max))
       vec))

(defn lines
  [points]
  (mapcat
   (fn [[x0 y0 :as start]
        [x1 y1 :as end]]
     (let [x-min (min x0 x1)
           y-min (min y0 y1)
           x-max (max x0 x1)
           y-max (max y0 y1)]
       (for [x (range x-min (inc x-max))
             y (range y-min (inc y-max))]
         [x y]))) (butlast points) (rest points)))

(defn rock-point
  [rock-paths]
  (mapcat lines rock-paths))

(defn draw-rock
  [terrain rock-points]
  (reduce (fn [terrain [x y]]
            (assoc-in terrain [y x] "#"))
          terrain rock-points))

(defn next-sand
  [terrain x y]
  (let [next-row (get terrain (inc y))]
    (->> (drop (dec x) next-row)
         (take 3))))

(defn next-move-pos
  [three-sand]
  (cond
    (= (second three-sand) ".")
    {:dx 0 :dy 1}

    (and
     (#{"o" "#"} (second three-sand))
     (= (first three-sand) "."))
    {:dx -1 :dy 1}

    (#{"."} (nth three-sand 2))
    {:dx 1 :dy 1}

    :else
    {:dx 0 :dy 0}))

(comment
  (next-sand [["." "." "." "o" "." "."]
              ["1" "2" "3" "4" "5" "6"]] 3 0)

  (next-move-pos ["." "." "."])
  (next-move-pos ["." "#" "."])
  (next-move-pos ["#" "#" "."])
  (next-move-pos ["#" "#" "#"]))


(defn move-next
  [{terrain :terrain
    y-max :y-max
    move :move
    {cx :x cy :y} 
    :sand :as status}]
  (let [three-sand (next-sand terrain cx cy)
        {dx :dx dy :dy} (next-move-pos three-sand)]
    (println "move to y : " (+ cy dx))
    (cond
      (:finished move)
      status

      (= (+ dy cy) y-max)
      (assoc status :move :finished)

      (zero? dy)
      (-> status
          (assoc-in [:terrain cy cx] ".")
          (assoc-in [:terrain (+ cy dy) (+ cx dx)] "o")
          (assoc-in [:sand :x] 500)
          (assoc-in [:sand :y] 0))
      :else
      (-> status
          (assoc-in [:terrain cy cx] ".")
          (assoc-in [:terrain (+ cy dy) (+ cx dx)] "o")
          (assoc-in [:sand :x] (+ cx dx))
          (assoc-in [:sand :y] (+ cy dy))))))


  (comment
    (def rock-paths
      (parse-input example))

    (def m-size (map-size rock-paths))
    (def terrain-map
      (let [{:keys [x-max y-max]} m-size]
        (init-terrain-map x-max y-max)))

    (def rock-points (rock-point rock-paths))
    (def terrained (draw-rock terrain-map rock-points))

    (def status
      (merge {:sand {:x 500
                     :y 0}
              :move :moving
              :terrain terrained}
             m-size))
    (last (take 1000 (iterate move-next status)))

    (drop-while #((complement #{:finished}) (:move %)) (iterate move-next status))
    )





  (defn stopped?
    [[x0 y0] [x1 y1]]
    (= [x0 y0] [x1 y1]))

  (comment
    (next-move-pos
     [1 2] [\. \# \.])
    (next-move-pos
     [1 2] [\. \. \.])

    (defn explore
      (let [])))



  (comment
    (draw-rock
     (init-terrain-map {:x-max 3 :y-max 3})
     [[[0 2] [0 3] [1 3]] [[2 1] [2 2]]]))
                 
                 
                 
  
  