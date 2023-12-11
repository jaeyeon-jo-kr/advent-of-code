

(ns day2
  (:require [clojure.string :as string]))

(def problem (slurp "../input.txt"))

(def max-loaded
  {:red 12
   :green 13
   :blue 14})

(defn read-cube
  [cube]
  (let [[cnt color]
        (string/split (string/trim cube) #" ")]
    [(keyword color)
     (read-string cnt)]))

(defn read-cube-set
  [cube-set]
  (->> (string/split cube-set #",")
       (map read-cube)
       (into {})))

(defn sum-cube-set
  [cube-sets]
  (reduce
   (fn [current {:keys [red blue green]}]
     (-> current
         (update :red + (or red 0))
         (update :blue + (or blue 0))
         (update :green + (or green 0))))
   {:red 0 :blue 0 :green 0}
   cube-sets))

(defn possible?
  [cube-lst]
  (every? (fn [{:keys [red blue green]}]
           (and (<= (or red 0)  (:red max-loaded))
                (<= (or blue 0) (:blue max-loaded))
                (<= (or green 0) (:green max-loaded)))
           ) cube-lst))

(defn read-game
  [game]
  (->> (string/split game #";")
       (map read-cube-set)))
  
(defn round-cnt
  [info]
  (-> (string/split info #" ")
      second
      read-string))




(comment
  (round-cnt "Game 30")
  )

(defn read-round
  [line]
  (let [[round game]
        (string/split line #":")
        rnd-cnt (round-cnt round)]
    [rnd-cnt (-> game
                 read-game
                 possible?)]))

(defn sum-possible
  [rounds]
  (->> (filter second rounds)
       (map first)
       (reduce +)))

(defn solve
  [input]
  (->> (string/split-lines input)
       (map read-round)
       sum-possible))

(def sample
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn power-of-cubes
  [cube-sets]
  (->>  (reduce (fn [cur {:keys [red blue green]}]
            (-> cur
                (update :red max (or red 0))
                (update :blue max (or blue 0))
                (update :green max (or green 0))))
          {:red 0 :blue 0 :green 0}
          cube-sets)
        vals
        (reduce *)))

(defn solve2
  [input]
  (->> (string/split-lines input)
       (map
        (comp
            power-of-cubes
            read-game
            second
            (string/split % #":")))
       (reduce +)))



(comment
  (solve sample)
  (solve problem)
  (solve2 sample)
  (solve2 problem))
