(ns aoc.day9 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def X 0)
(def Y 1)
(def initial [0 0])
(def tail-visited (atom #{}))

(def initial-status 
  {:H initial
   :T initial})

(defn touched? 
  [{[hx hy] :H
    [tx ty] :T
    :as _}]
  (and (<= (dec hx) tx (inc hx))
       (<= (dec hy) ty (inc hy))))

(defn next-tail-position
  [{[hx hy] :H
    [tx ty] :T}]
  (let [distance [(- hx tx) (- hy ty)]
        [dx dy] (case distance
              [2 1] [1 1]
              [2 0] [1 0]
              [2 -1] [1 -1]
              [1 2] [1 1]
              [1 -2] [1 -1]
              [0 2] [0 1]
              [0 -2] [0 -1]
              [-1 2] [-1 1]
              [-1 -2] [-1 -1]
              [-2 -1] [-1 -1]
              [-2 0] [-1 0]
              [-2 1] [-1 1]
              [0 0])]
    [(+ tx dx) (+ ty dy)]))


(defn tail-step
  [status]
  (if (touched? status)
    status
    (let [[tx ty] (next-tail-position status)]
      (swap! tail-visited #(conj % [tx ty]))
      (-> status
          (assoc-in [:T 0] tx)
          (assoc-in [:T 1] ty)))))

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
(defn steps
  [status [direct step-count]]
  (->> (iterate (partial step direct) status)
       (take (inc step-count))
       last))

(defn read-path
  [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn solve-all
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

(def visited-2 (atom #{}))

(def initial-status-2
  (repeat 10 initial))

(def move-map
  {:D [Y dec]
   :U [Y inc]
   :L [X dec]
   :R [X inc]})

(defn head-step-2
  [head direct]
  (let [[axis mod-fn] (direct move-map)]
    (update head axis mod-fn)))

(defn next-tail-position-2
  [[hx hy] [tx ty]]
  (let [distance [(- hx tx) (- hy ty)]
        [dx dy] (case distance
                  [2 1] [1 1]
                  [2 0] [1 0]
                  [2 -1] [1 -1]
                  [2 2] [1 1]
                  [2 -2] [1 -1]
                  [1 2] [1 1]
                  [1 -2] [1 -1]
                  [0 2] [0 1]
                  [0 -2] [0 -1]
                  [-1 2] [-1 1]
                  [-1 -2] [-1 -1]
                  [-2 -1] [-1 -1]
                  [-2 0] [-1 0]
                  [-2 1] [-1 1]
                  [-2 -2] [-1 -1]
                  [-2 2] [-1 1]
                  [0 0])]
    [(+ tx dx) (+ ty dy)]))

(defn tail-step-2
  [tail prev] 
  (let [[tx ty] (next-tail-position-2 prev tail)]
    (-> tail
        (assoc X tx)
        (assoc Y ty))))

(defn step-2
  [direct status]
  (let [head (first status)
        tails (next status)]
    (->> (reduce (fn [prev-tails tail]
                   (when (= 9 (count prev-tails))
                     (swap! visited-2 #(conj % tail)))
                   (-> tail 
                       (tail-step-2 (first prev-tails))
                       (cons prev-tails)))
                 [(head-step-2 head direct)] tails)
         reverse)))

(defn steps-2
  [status [direct step-count]]
  (->> (iterate (partial step-2 direct) status)
       (take (inc step-count))
       last))

(defn solve-all-2
  [input]
  (reset! visited-2 #{})
  (->> input
       read-path
       (reduce steps-2 initial-status-2)))


(comment 
  (do 
    (reset! visited-2 #{})
    (->> (read-path "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20")
         (reduce steps-2 initial-status-2))
    (->> @visited-2
         count)
    
    )
  

  (do 
    (reset! visited-2 #{})
    (->> '([:R 4] [:U 4] [:L 3] [:D 1] [:R 4] [:D 1] [:L 5] [:R 2])
         (reduce steps-2 initial-status-2))
    (->> @visited-2)
    
    )
  (-> (slurp "./day9_input.txt")
      solve-all-2)
  (step-2 :U initial-status-2)
  (steps-2 initial-status-2 [:U 11])
  (->> (slurp "./day9_input.txt")
       read-path
       (take 10))
  (do
    (-> (slurp "./day9_input.txt")
        solve-all-2)
    (-> @visited-2
        count))

  )