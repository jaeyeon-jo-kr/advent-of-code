(ns aoc.day5
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn ->crates
  [coll] 
  (let [ch (second coll)]
    (when (not= ch \space)
      (keyword (str ch)))))

(defn stack-of-create
  [text]
  (->> (str/split-lines text)
       (butlast)
       (map (comp (partial map ->crates) 
                  (partial partition-all 3 4)))
       (apply map (fn [& coll] (keep identity coll)))))

(defn ->command
  [text]
  (->> (str/split text #" ")
       (partition-all 2)
       (reduce (fn [m [k v]]
                 (assoc m (keyword k) (read-string v))) {})))

(defn moving-piece 
  [stack move from]
  (->> (nth stack (dec from))
       (take move)
       reverse))

(defn move
  "move piece from stack"
  [stack {:keys [move from to]}]
  (-> (vec stack)
      (update (dec from) (partial drop move))
      (update (dec to) 
              (partial concat (moving-piece stack move from)))
       seq))

(defn parse-commands
  [commands]
  (->> (str/split-lines commands)
       (map ->command)))

(defn parse
  [text]
  (-> text
      (str/split #"\n\n")
      (update 0 stack-of-create)
      (update 1 parse-commands)))

(defn top-of-stacks
  [[stack commands]]
  (->> (reduce move stack commands)
       (map (comp name first))
       (apply str)))

(defn move-2
  "move piece from stack"
  [stack {:keys [move from to]}]
  (-> (vec stack)
      (update (dec from) (partial drop move))
      (update (dec to) 
              (->> (nth stack (dec from))
                   (take move)
                   (partial concat)))
       seq))

(defn top-of-stacks-2
  [[stack commands]]
  (->> (reduce move-2 stack commands)
       (map (comp name first))
       (apply str)))