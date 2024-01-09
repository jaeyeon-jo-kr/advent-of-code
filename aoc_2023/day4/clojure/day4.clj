(ns day4)

(defn win-nums [w-nums have-nums]
  (filter (set w-nums) have-nums))

(defn points [w-nums have-nums]
  (let [cnt (count (win-nums w-nums have-nums))]
    (if (zero? cnt)
      cnt
      (->> (repeat (dec cnt) 2)
           (reduce * 1)))))

(defn read-row
  [line]
  (let [[_ w-nums have-nums]  (clojure.string/split line #":|\|")]
    [(->> (clojure.string/split (clojure.string/trim w-nums) #" +")
          (map read-string))
     (->> (clojure.string/split (clojure.string/trim have-nums) #" +")
          (map read-string))]))

(defn read-input [input]
  (->>   (clojure.string/split-lines input)
         (map read-row)))

(defn solve [input]
  (->>    (map (fn [[w h]] (points w h)) (read-input input))
          (apply + 0)))

(def r (-> (slurp "../sample.txt") solve ))
(map (fn [[w h]] (points w h)) r)
(solve (slurp "../sample.txt"))
(solve (slurp "../input.txt"))

(defn read-row-round
  [line]
  (let [[round w-nums have-nums]
        (clojure.string/split line #":|\|")]
      [(read-string (re-find  #"\d+" round))
       (->> (clojure.string/split (clojure.string/trim w-nums) #" +")
          (map read-string))
       (->> (clojure.string/split (clojure.string/trim have-nums) #" +")
            (map read-string))]))

(defn copies [wins]
  (map (fn [[id wins]]
         [id (range (inc id) (+ id wins 1))]) wins))

(defn counts [copies]
  (let [scores (mapv (fn [_] 1) copies)]
    (reduce (fn [s [i copies]]
            (reduce (fn [s j]
                      (update s (dec j) + (get s (dec i))))
                    s copies)) scores copies)))

(defn read-input-p2 [input]
  (->> (clojure.string/split-lines input)
       (map read-row-round)
       (map (fn [[round w-nums have-nums]]
              [round (count (win-nums w-nums have-nums))]))
       copies
       counts))



(comment
  (def readed-input (read-input-p2 (slurp "../sample.txt")))
  (def fround   (first readed-input))
  (win-nums (second fround) (nth fround 2))
  (apply winning-card-nums fround)
  
(def result  (->>  (playing-game [] (read-input-p2 (slurp "../input.txt")))
                   (take 204)
                   (apply + )))
(println result)
(apply +  (playing-game [] (read-input-p2 (slurp "../sample.txt"))))
  total-cnt (-> (get deck round 0) inc)
  result (copy-card-nums round w-nums have-nums)
  )



  



