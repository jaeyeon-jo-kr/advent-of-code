(ns aoc.day12
  (:require [clojure.string :as str]
            [clojure.string :as s]))

(def file "day12_sample.txt")

(defn read-content
  [texts]
  (->> (str/split-lines texts)
       (mapv vec)))

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
  (and (<= 0 y (dec (count routes)))
       (<= 0 x (dec (count (first routes))))))

(comment 
  (in-routes?
   [[0 0] [1 1]] 1 1)
(count  [[0 0] [1 1]])
  )

(defn height
  [s]
  (int  (case s \E \z \S \a s)))

(defn steppable?
  [current next]
  (<= (dec (height next)) (height current) (height next)))

(defn remove-visited
  [visited neighbours]
  (->> neighbours
       ((fn [d] (println "remove-visited before :" d) d))
       (remove (fn [[x y]]
                 (some (comp #{[x y]} (juxt first second)) visited)))
       ((fn [d] (println "remove-visited after :" d) d))))


(defn neighbours
  [routes x y]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       #_((fn [d] (println "in-routes before :" routes d) d))
       (filter (fn [[nx ny]] (in-routes? routes nx ny)))
       #_((fn [d] (println "in-routes after :" d) d))
       (filter (fn [[nx ny]] (steppable?
                              (get-in routes [y x])
                              (get-in routes [ny nx]))))
       #_((fn [d] (println "steppable after :" d) d))))


(defn update-distance
  [distances x y next-dist]
  (update-in distances [y x]
          (fn [dist]
            (min dist next-dist))))

(defn init-distance
  [routes [x y :as _start]]
  (-> (mapv (fn [row] (mapv (constantly Integer/MAX_VALUE) row)) routes)
      (assoc-in [y x] 0)))


(defn solve
  [[queue routes distances]]
  (if-let [{[x y] :node
            visited :visited} (peek queue)]
    (if-let [neighbours
             (->> (neighbours routes x y)
                  (remove-visited visited)
                  seq)]
      [(apply conj (pop queue) 
              (->> neighbours
                   (map #(assoc {} 
                                :node %
                                :visited (conj visited [x y])))))
       routes

       (->> neighbours
            (reduce
             (fn [distances [nx ny]]
               (println "neighbor : " nx ny)
               (println "distance : " (get-in distances [y x]))
               (update-distance
                distances nx ny
                (-> distances 
                    (get-in [y x])
                    inc)))
             distances))]
      [(pop queue) routes distances])
    [queue routes distances]))


(comment 
  (def example "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")
  (def routes (read-content example))
  (def queue [{:node (find-start routes)
               :visited []}]) 
  (def distances (init-distance routes (find-start routes)) )

  [[\S \a \b \q \p \o \n \m] 
   [\a \b \c \r \y \x \x \l]
   [\a \c \c \s \z \E \x \k]
   [\a \c \c \t \u \v \w \j]
   [\a \b \d \e \f \g \h \i]]
  [[0 1 2 21 20 19 18 17] 
   [1 2 9 22 31 30 29 16]
   [2 7 8 23 32 33 28 15]
   [3 6 7 24 25 26 27 14]
   [4 5 8 9 10 11 12 13]]

  (->> (iterate solve [queue routes distances])
       (take 100)
       last)
  
       
       (solve [queue routes  distances])
       
       

       (pop [1])
       (steppable? 1 0)
       (-> (slurp file)
           read-content
           find-start)
       )



