(ns aoc.day12
  (:require [clojure.string :as str]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def file "day12_sample.txt")

(defn read-content
  [texts]
  (->> (str/split-lines texts)
       (mapv (comp vec str/trim))))

(defn height
  [s]
  (case s
    \E (inc (int \z))
    \S (dec (int \a))
    (int s)))

(defn steppable?
  [from to] 
  (and (not= from \E) (not= to \S)
       (<= (dec (height to)) (height from) (height to))))

(comment 
  (steppable? \S \b)
  (steppable? \z \E)
  (->> (range (int \a)
              (inc (int \z)))
       ((juxt butlast rest))
       (apply map (fn [a b] (steppable? a b)))
       
       
       )

  )

(defn keywordize
  [x y]
  (keyword (str x "." y)))

(defn unkeywordize
  [pt-key]
  (->> (clojure.string/split (name pt-key) #"\.")
       (mapv read-string)))

(defn generate-graph
  [char-map]
  (let [height (count char-map)
        width (count (first char-map))]
    (->>
     (for [r (range height)
           c (range width)]
       (let [from  (get-in char-map [r c])]
         [(keywordize r c)
          (->> [[-1 0] [1 0] [0 1] [0 -1]]
               (keep
                (fn [[y x]]
                  (let [x (+ c x) y (+ r y)
                        to (get-in char-map [y x])]
                    (when (and
                           (<= 0 y (dec height))
                           (<= 0 x (dec width))
                           from to
                           (steppable? from to)) 
                      [(keywordize y x) 1]))))
               (into {}))]))
     (into {})
     graph/weighted-graph)))

(defn find-start-end
  [path-map]
  (->> path-map
       (keep-indexed
        (fn [y row]
          (->> (keep-indexed
                (fn [x ch]
                  (case ch
                    \E [:E [y x]]
                    \S [:S [y x]]
                    nil)) row)
               seq
               )))
       (apply concat)
       (into {})))

(comment 
  (into {} [[1 2] [3 4]])
  (count '(1 2 3)) 
  (def g (generate-graph [[\S \b \c] 
                          [\a \b \c] 
                          [\c \c \d]]))
  (alg/dijkstra-path g :0.0 :2.2)
  
  
  (def path-map (read-content (slurp file)))
  (def start-end (find-start-end path-map))
  (def start (apply keywordize (:S start-end)))
  (def end (apply keywordize (:E start-end)))
  (def graph (generate-graph path-map))
  (alg/dijkstra-path graph start end)
  (alg/dijkstra-path graph :20.0 :20.78)
  (alg/dijkstra-path graph :20.78 :20.88)
  (alg/dijkstra-path graph :20.89 :20.88)
  (alg/dijkstra-path graph :19.82 :20.88) 
  :g 
  (alg/dijkstra-path graph :19.79 :19.82)
  (alg/dijkstra-path graph :12.83 :11.83)
  (alg/dijkstra-path graph :12.83 :19.103)
  (alg/dijkstra-path graph :19.103 :29.99 )
  (alg/dijkstra-path graph :27.98 :27.86)
  (alg/dijkstra-path graph :27.98 :20.88)
  (alg/dijkstra-path graph :20.0 :12.95)
  


  (io/view graph)

  ;; End is unreachable.
  )







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