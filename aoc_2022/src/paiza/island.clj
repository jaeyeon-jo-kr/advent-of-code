(ns paiza.island)

; 自分の得意な言語で
; Let's チャレンジ！！
(require '[clojure.string :as string])


(defn read-input
    []
    (let [[_col row] (->> (clojure.string/split (read-line) #" ")
                         (map read-string))]
         (repeatedly row
                     (fn []
                       (->> (clojure.string/split (read-line) #" ")
                            (map read-string))))))

(defn read-input-str
  [input-str]
  (let [lines (clojure.string/split-lines input-str)
        [_width _height] (->> (clojure.string/split (nth lines 0) #" ")
                       (map read-string))]
    (->> (drop 1 lines)
         (map (comp #(map read-string %) #(clojure.string/split % #" "))))))

(defn move
  [{:keys [buffer env-map results] :as stat}]
  (cond
    (and (= (count buffer) (count (last env-map)))
         (= (inc (count results)) (count env-map)))
    (-> (assoc stat :end? true)
        (update :results conj buffer))

    (= (count buffer) (count (last env-map)))
    (-> (update stat :results conj buffer)
        (assoc :buffer []))
    
    :else
    stat))


(defn find-land
  [{:keys [env-map results buffer color-count] :as stat}]
  (let [left (or (last buffer) 0)
        up (or (-> (last results) (nth (count buffer))) 0)
        cur (-> (nth env-map (count results)) 
                (nth (count buffer)))]
    (cond
      (zero? cur)
      (update stat :buffer #(conj % 0))

      (= left up 0)
      (-> stat
          (update :buffer #(conj % (inc color-count)))
          (update :land-count inc)
          (update :color-count inc))

      (or (and (pos? left) (zero? up))
          (and (pos? up) (zero? left))
          (= left up))
      (-> stat
          (update :buffer #(conj % (max up left))))

      :else
      (-> stat
          (update :buffer
                  (comp #(mapv (fn [n] (if (= n left) up n)) %)
                        #(conj % up)))
          (update :land-count dec)))))

(defn step
  [stat]
  (-> stat find-land move))

(defn print-env
  [env]
  (println "-------")
  (doseq [item env]
    (doseq [n item]
      (print (format " %03d" n)))
    (println))
  (println "-------"))


(defn solve [input]
  (let [status {:land-count 0
                :color-count 0
                :env-map input
                :results []
                :buffer []}]
    (loop [status status]
      (if-not (:end? status)
        (recur (step status))
        (do (print-env (:results status))
            (->> status
                 :land-count))))))
             

(def mondai (slurp "./test-case_mondai__1.txt"))
(def mondai2 (slurp "./test-case_mondai__2.txt"))
(def mondai3 (slurp "./test-case_mondai__3.txt"))
(def mondai4 (slurp "./test-case_mondai__4.txt"))


(comment 
  (-> (read-input) solve)
  (-> mondai2
      read-input-str
      solve
      #_print-env
      )
  (-> mondai
      read-input-str
      solve)
  (-> mondai3
      read-input-str
      solve)
  (-> mondai4
      read-input-str
      solve)
  (-> "3 3\n0 1 1\n0 1 0\n1 1 1"
      read-input-str
      solve)
  
  (-> (string/join
       "\n"
       ["4 4"
        "0 1 0 1"
        "0 1 1 0"
        "0 1 0 1"
        "1 0 1 1"])
      read-input-str
      solve)
  

  

  "0 1 1 0
   1 0 1 0
   1 0 0 0
   0 0 1 1
   0 1 1 1
   "
  (-> mondai4
      read-input-str
      solve)
  )

