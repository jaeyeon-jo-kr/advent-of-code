(ns aoc.day12
  (:require [clojure.string :as str]))

(def start \S)
(def end \E)


(defn parse-line
  [line]
  (->> (str/trim line)
       (map-indexed
        (fn [x signal]
          {:x x
           :signal signal
           :eval
           (case signal
             \S (int \a)
             \E (int \z)
             (int signal))}))))

(def directions
  [{:dx 1 :dy 0 :dir \>}
   {:dx -1 :dy 0 :dir \<}
   {:dx 0 :dy 1 :dir \v}
   {:dx 0 :dy -1 :dir \^}])


(defn stepable?
  [{curr-eval :eval} {next-eval :eval}]
  (when (and curr-eval next-eval)
    (<= (dec next-eval) curr-eval next-eval)))

(defn find-node
  [x y nodes]
  (some #(when (= [x y] ((juxt :x :y) %)) %) nodes))

(defn end-node?
  [{:keys [signal] :as _node}]
  (= signal \E))

(defn begin-node?
  [{:keys [signal] :as _node}]
  (= signal \S))

(defn init-edges
  [nodes]
  (->> nodes
       (mapcat (fn [{:keys [x y] :as node}]
                 (->> directions
                      (keep (fn [{:keys [dx dy dir]}]
                              (let [tx (+ x dx)
                                    ty (+ y dy)
                                    next-node (find-node tx ty nodes)]
                                (when (and (stepable? node next-node)
                                           (not (end-node? node))
                                           (not (begin-node? next-node)))
                                  (assoc {}
                                         :x x
                                         :y y
                                         :tx tx
                                         :ty ty
                                         :dir dir))))))))))

(defn init-nodes
  [text]
  (->> text
       (str/split-lines)
       (map-indexed
        (fn [y line]
          (->> (parse-line line)
               (map #(assoc % :y y)))))
       flatten))



(defn init-current-node
  [nodes]
  (-> (filter #(#{\S} (:signal %)) nodes)
      first))

(defn initialize
  [text]
  (as-> {} s
    (assoc s :nodes (init-nodes text))
    (assoc s :edges (init-edges (:nodes s)))))

(defn remove-edges-by-visited
  [visited edges] 
  (for [{tx :tx ty :ty :as edge} edges
        {x :x y :y} visited
        :when (not= [tx ty] [x y])]
    edge))



(defn find-next-edges
  [{x :x y :y :as _node} edges visited]
  (->> edges
       (filter (comp #{[x y]} (juxt :x :y)))
       (remove-edges-by-visited visited)
       distinct
       seq))

(defn target-node
  [{x :tx y :ty :as _edge} nodes]
  (->> nodes
       (filter (comp #{[x y]} (juxt :x :y)))
       first))


(defn next-step-fn
  [nodes edges]
  (fn [{min-step :min-step
        work-queue :work-queue :as work-status}]
    (let [{visited :visited
           steps :steps
           current :current :as status}
          (peek work-queue)

          visited (conj visited current)
          status (assoc status :visited visited)

          next-edges
          (seq (find-next-edges current edges visited))
          
          work-queue (update work-status :work-queue pop)]
      (println "current : " current)
      (println "work queue conut : " (count work-queue))
      (cond
        (nil? status)
        min-step

        (end-node? current)
        (-> work-status
            (update  :min-step #(min steps %))
            )
        

        (nil? next-edges)
        work-status

        :else
        (->> next-edges
             (map (fn [edge]
                    (-> status
                        (assoc :current (target-node edge nodes))
                        (update :steps inc))))
             (apply conj work-queue)
             (assoc work-status :work-queue))))))

(comment
  (mapcat identity [[1 2] [2 3] [2]])

  (conj #{1 2} 3)
  (let [{:keys [nodes edges]}
        (initialize "SabcdefghijklmnopqrstuvwxyzE")
        init-status {:visited []
                     :current (init-current-node nodes)
                     :steps 0}
        work-status {:min-step 0
                     :work-queue [init-status]}
        next-step (next-step-fn nodes edges)]
    (->  (next-step work-status)
         next-step
         next-step
         
         next-step
         next-step
         next-step
         )
    )
  
  (->> (initialize "Sabqponm
      abcryxxl
      accszExk
      acctuvwj
      abdefghi")
       find-minimal-step)
  (->> (slurp "day12_sample.txt")
       initialize
       find-minimal-step)
  )


