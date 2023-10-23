(ns paiza.mod7
  (:require [clojure.string :as string]))

; 自分の得意な言語で
; Let's チャレンジ！！
(require '[clojure.string :as string])


(defn read-input
    []
    (let [n (read-string (read-line))]
      (repeatedly n
                  (fn []
                    (read-string (read-line))))))

(defn read-str
  [input-str]
  (let [[_n & numbers] (->> (string/split-lines input-str)
                          (map read-string))]

    numbers))


(defn get-mod7-combs
  []
  (->> (for [i (range 7)
             j (range 7)
             k (range 7)
             :when (zero? (mod (+ i j k) 7))]
         [i j k])
       (map sort)
       distinct))

(defn freq-mod
  [comb]
  (->> (frequencies comb)
       vals
       (map (fn [v] (reduce * (range 1 (inc v)))))
       (reduce *)))

(defn combination
  [comb vals]
  (-> (reduce (fn [[vals cnt] n] 
                [(update vals n #(max 0 (dec %))) (* cnt (get vals n))]) 
              [vals 1] comb)
      second
      (quot (freq-mod comb))))

(defn ->mod
  [nums]
  (reduce (fn [coll n]
            (update coll n inc))
   (vec (repeat 7 0))
   (mapv #(mod % 7) nums)))


(defn find-mod7
  [nums]
  (if (< (count nums) 3)
    0
    (->> (map #(combination % (->mod nums)) (get-mod7-combs))
         (reduce +))))

(defn tc-1 
  []
  (->> (range (->> (repeat 2)
                   (take 32)
                   (reduce *)))
       (take-last 100000)))

(comment
  
  (repeat 7 0)
  (->mod [1 2 3 4 5 6 7 8 9 10])
  (combination [0 2 5] (->mod [1 2 3 4 5 6 7 8 9 10]))
  (map #(combination % (->mod [1 2 3 4 5 6 7 8 9 10])) (get-mod7-combs))
  (find-mod7 [1 2 3 4 5 6 7 8 9 10])
  (time (tc-1))
  (time (find-mod7 (tc-1)))
  
  (mod 10 7)
  (reduce (fn [a b] b) [0] [1 2])
  (freq-mod [0 0 0])
  (combination [0 1 6] [3 1 2 3 4 5 6])
  
  )

(comment 
  (->> (repeat 2)
       (take 32)
       (reduce *))
  (drop-last 2 [2 3 4])
  (butlast [1 2 3])
  (zipmap (range) [1 2 3]) (read-string "1
                       2"))

