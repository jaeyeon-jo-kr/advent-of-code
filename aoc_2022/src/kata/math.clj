(ns kata.math 
  (:require [clojure.string :as s]))

(def token-type [:start :number :+ :- :* :/ :lp :rp])

(defn next-state
  [ch state]
  (case ch
    \( :lp
    \) :rp
    \+ :+
    \- :-
    \* :*
    \/ :/
    \. :.
    :number))

(defn append-buf
  [state tokens buf]
  (if (seq buf)
    (conj tokens {:type state
                  :value (-> (apply str buf)
                             read-string)})
    tokens))

(defn parse-text
  ([{:keys [text state buf tokens] :as s}]
   (let [h (first text)
         r (rest text)
         next-state (next-state h state)]
     (cond
       (:end state)
       s

       (nil? h)
       {:text r
        :state :end
        :buf []
        :tokens (-> (append-buf state tokens buf)
                    (conj {:type next-state}))}

       (#{:+ :- :* :/ :lp :rp} next-state)
       {:text r
        :state next-state
        :buf []
        :tokens (-> (append-buf state tokens buf)
                    (conj {:type next-state}))}
       

       :else
       {:text r
        :state next-state
        :buf (conj buf h)
        :tokens tokens}))))

(defn gen-parse-tree
  [{:keys [tokens cur-id parent-id tree] :as state}]
  (let [{:keys [type value]} tokens]
    (cond
      (:lp type)
      {:tokens (rest tokens)
       })
    
    )


  )

(defn solve
  [text]
  (iterate parse-text {:text text
                       :state :start
                       :buf []
                       :tokens []}))

(comment
  (take 10 (solve "11.3+22+44"))
  (take 3 (solve "(1+1)"))
  )