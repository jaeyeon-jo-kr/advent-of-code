(ns aoc.day12
  (:require [clojure.string :as str]))

(def file "day12_sample.txt")

(defn read-content
  [texts]
  (->> (str/split-lines texts)
       (mapv (comp vec str/trim))))

(comment 
  (vector '(1 2 3)))

(defn find-start
  [routes]
  (let [find-col
        (fn [x val] (when (= \S val) x))
        find-row
        (fn [y row]
          (when-first [x (keep-indexed find-col row)]
            [x y]))]
    (first (keep-indexed find-row routes))))

(defn in-routes?
  [routes x y]
  (-> (and (<= 0 y (dec (count routes)))
           (<= 0 x (dec (count (first routes)))))
      #_((fn [r] (println "in routes?" [x y r]) r))))  

(defn height
  [s]
  (int (case s \E \z \S \a s)))

(defn steppable?
  [current next]
  (-> (<= (dec (height next)) (height current) (height next))
      #_((fn [r] (println "steppable?" [current next r])r))))

(defn remove-visited
  [distances cx cy neighbours]
 #_ (println "remove from :"
           [cx cy (get-in distances [cy cx])])
  (->> neighbours
       (remove
        (fn [[x y]]
          (let [result
                (<= (get-in distances [y x])
                    (inc (get-in distances [cy cx])))]
         #_  (if result
              (println "\tremoved : " [x y (get-in distances [y x])])
              (println "\tnot removed : " [x y (get-in distances [y x])]))
            result)))))


(defn neighbours
  [routes x y]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)])) 
       (filter (fn [[nx ny]] (in-routes? routes nx ny)))
       (filter (fn [[nx ny]] (steppable?
                              (get-in routes [y x])
                              (get-in routes [ny nx]))))))

(defn init-distance
  [routes [x y :as _start]]
  (-> (mapv (fn [row] (mapv (constantly Integer/MAX_VALUE) row)) routes)
      (assoc-in [y x] 0)))


(defn print-situation
  [distances routes cx cy]
  (print "c : ")
  (->> (map (fn [[x y]]
              [(+ x cx)
               (+ y cy)])
            [[0 0] [1 0] [-1 0] [0 1] [0 -1]])
       (map (fn [[x y]]
              (let [dist (get-in distances [y x])
                    r    (get-in routes [y x])]
                (print [x y r dist ]  " "))))
       doall)
  (println))

(comment 
  (get-in [] [1 2])
  
  )

(defn solve
  [[queue routes distances]]
  (if-let [[x y] (last queue)]
    (let [_ (print-situation distances routes x y)
          dist (get-in distances [y x])]
      
      (if-let [neighbours
               (->> (neighbours routes x y)
                    (remove-visited distances x y)
                    seq)]
        [(reduce (fn [q n] (cons n q))
                 (butlast queue)
                 neighbours)
         routes
         (->> neighbours
              (reduce
               (fn [curr-dist [nx ny]]
                 (update-in curr-dist [ny nx] #(min % (inc dist))))
               distances))
         (println "n : " neighbours)]
        [(butlast queue) routes distances
         (println "no n")]))
    [queue routes distances]))

(comment 
  (cons 3 '(1 2))
  (last '(3 1 2))
  (butlast '(3 1 2))
  (butlast '())

  (peek [1 2 3])
  (pop [1 2 3])
  )


(defn final-pos
  [routes]
  (let [find-col
        (fn [y] (fn [x val] (when (= \E val) [x y])))

        find-row
        (fn [y row]
          (->>  (keep-indexed (find-col y) row)
                first))]
    
    (-> (keep-indexed find-row routes)
        first)))

(defn final-distance 
  [x y distances]
  (get-in distances [y x]))

(defn solve-problem
  [text]
  (let [routes (read-content text)
        queue (list (find-start routes))
        distances (init-distance routes (find-start routes))]
    (loop [[queue routes distances]
           [queue routes distances]] 
      (if (seq queue) 
        (recur (solve [queue routes distances])) 
        (let [[x y] (final-pos routes)]
          (final-distance x y distances))))))

(comment
  (solve-problem
   "Sabqponm
                                abcryxxl
                                accszExk
                                acctuvwj
                                abdefghi")
  
  (-> (solve-problem
       "Sabqponmllc
        abcryxxlllb
        accszExklla
        acctuvwjllp
        abdefghillk")) 
  
  (let [routes (read-content (slurp file))]
    (println (get-in routes [31 96]))
    (println (get-in routes [32 96]))
    (println (get-in routes [31 95]))
    (println (get-in routes [30 96]))
    (println (get-in routes [31 97]))
    )
  
  (-> (slurp file)
      solve-problem)
  Integer/MAX_VALUE
  )