(ns aoc.day9 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def X 0)
(def Y 1)
(def initial [0 0])

(def initial-status 
  {:H initial
   :T initial
   :visited {[0 0] 1}})

(defn touched? 
  [{[hx hy] :H
    [tx ty] :T
    :as _}]
  (and (<= (dec hx) tx (inc hx))
       (<= (dec hy) ty (inc hy))))

(defn next-tail-position
  [{[hx hy] :H
    [tx ty] :T}]
  [(if-not (= hx tx)
     (-> (- hx tx)
         (/ (abs (- hx tx)))
         (+ tx))
     tx)
   (if-not (= hy ty)
     (-> (- hy ty)
         (/ (abs (- hy ty)))
         (+ ty))
     ty)])


(defn tail-step
  [status]
  (if (touched? status)
    status
    (let [[tx ty] (next-tail-position status)]
      (-> status
          (assoc-in [:T 0] tx)
          (assoc-in [:T 1] ty)
          (update-in [:visited [tx ty]] (fnil inc 0))))))

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


(defn step
  [direct status]
  (-> (head-step status direct)
      tail-step
      (doto (-> (dissoc :visited)
                println))))

(comment
  (doto 1
    (println))

  )

(defn steps
  [status [direct step-count]]
  (->> (iterate (partial step direct) status)
       (take (inc step-count))
       last))

(defn read-path
  [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn tail-visited
  [input]
  (->> input
       read-path
       (reduce steps initial-status)))

(defn print-point
  [visited]
  (let [x-max (->> visited
                   (map first)
                   (apply max))
        y-max (->> visited
                   (map second)
                   (apply max))
        x-min (->> visited
                   (map first)
                   (apply min))
        y-min (->> visited
                   (map second)
                   (apply min))
        visited-map
        (->> (->> (repeat ".")
                  (take (inc (- x-max x-min)))
                  vec)
             repeat
             (take (inc (- y-max y-min)))
             vec)]
    (->> (reduce (fn [visited-map [x y]]
                   (let [x (+ x x-min)
                         y (+ y y-min)]
                     (assoc-in visited-map [(- y-max y) x] "#")))
                 visited-map visited)
         (map (partial apply str))
         (interpose "\r\n")
         (reduce str))))


(comment
  (vec '(1 2 3))
  
  (-> "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
      tail-visited
      :visited
      keys
      print-point
      println-str
      )
  
  (-> (slurp "./day9_input.txt")
      tail-visited
      :visited
      keys
      print-point
      println-str) 
  

  (->> (slurp "./day9_input.txt")
      read-path
       (take 10))
  (-> (slurp "./day9_input.txt")
      tail-visited 
      )
)
