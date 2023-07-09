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
  (as-> {:steps 0
         :visited []} s
    (assoc s :nodes (init-nodes text))
    (assoc s :edges (init-edges (:nodes s)))
    (assoc s :current (init-current-node (:nodes s)))))

(defn find-next-edges
  [{x :x y :y :as _node} edges]
  (->> edges
       (filter (comp #{[x y]} (juxt :x :y)))
       seq))

(defn target-node
  [{x :tx y :ty :as _edge} nodes]
  (->> nodes
       (filter (comp #{[x y]} (juxt :x :y)))
       first))

(defn remove-visited-edge
  [edges {x :x y :y}]
  (remove (comp #{[x y]} (juxt :tx :ty)) edges))

(defn find-minimal-step
  [status]
  (let [node (:current status)
        nodes (:nodes status)]
    (if (end-node? node)
      (assoc status :found true)
      (if-let [next-edges (find-next-edges node (:edges status))]
        (->> next-edges
             (map (fn [edge]
                    (-> status
                        (assoc :current (target-node edge nodes))
                        (update :steps inc)
                        (update :edges #(remove-visited-edge % node))
                        find-minimal-step)))
             (filter :found)
             (sort-by :steps)
             first)
        (assoc status :found false)))))

(comment 
    (->> (initialize "SabcdefghijklmnopqrstuvwxyzE")
         
         )

  (->>(initialize "Sabqponm
      abcryxxl
      accszExk
      acctuvwj
      abdefghi")
   find-minimal-step)
  )


