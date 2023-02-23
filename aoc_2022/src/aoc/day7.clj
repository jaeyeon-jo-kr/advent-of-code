(ns aoc.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(defn action
  [command]
  (cond
    (str/includes? command "$ cd") :cd
    (str/includes? command "$ ls") :ls))

(defn change-directory
  [commands pwd tree]
  (let [param (str/replace (first commands) "$ cd " "")]
    (case param
      "/" [(rest commands)
           [:/]
           tree]

      ".." [(rest commands)
            (-> (butlast pwd)
                seq
                (or [:/])
                vec)
            tree]

      [(rest commands)
       (conj pwd (keyword param))
       tree])))

(defn create-file
  [spec]
  (let [[head tail] (str/split spec #" ")]
    (if (= head "dir")
      {(keyword tail) {:_size 0}}
      {(keyword tail) (read-string head)})))

(defn add-file
  [tree pwd file]
  (update-in tree pwd merge file))

(defn list-directory
  [commands pwd tree]
  
  (let [[contents rest-commands]
        (->> (rest commands)
             (split-with #(-> % first #{\$} not)))]
    [rest-commands
     pwd
     (->> contents
          (map create-file)
          (reduce (fn [tree file]
                    (add-file tree pwd file))
                  tree))]))

(defn process-commands
  [[commands pwd tree]]
  (case (action (first commands))
    :cd (change-directory commands pwd tree)
    :ls (list-directory commands pwd tree)
    [commands pwd tree]))

(defn all-process
  [commands]
  (loop [[commands pwd tree]
         [commands [:/] {:/ {}}]]
    (if (seq commands)
      (-> (process-commands [commands pwd tree])
          recur)
      tree)))

(defn dir-size
  [tree]
  (reduce-kv
   (fn [n _ v]
     (cond
       (map? v)
       (let [size (dir-size v)]
         (+ n size))

       (int? v)
       (+ n v))) 0 tree))

(defn dir-size-list
  [tree]
  (reduce-kv
   (fn [coll k v]
     (if (map? v)
       (->> coll
            (cons [k (dir-size v)])
            (concat (dir-size-list v)))
       coll))
   []
   tree))

(defn delete-directory
  [path tree]
  (update-in tree (butlast path)
             dissoc (last path)))

(defn read-file-input
  []
  (-> (slurp "./day7_input.txt")
      (str/split-lines)))

(defn solve-part1
  []
  (->> (read-file-input)
      all-process
      dir-size-list
       (filter #(-> % second (< 100000)))
       (map second)
       (reduce +)))

(defn solve-part2
  []
  (let [size-list
        (->> (read-file-input)
             all-process
             dir-size-list
             (sort-by second))
        total-size 
        (->> size-list
             (filter #(-> % first #{:/}))
             first
             second)]
    (->> size-list
         (map second)
         (drop-while #(< 40000000
                         (- total-size %)))
         (first))))

(comment
  (#{:/} :/)
  (solve-part2)
  (all-process
   ["$ ls"
    "dir a"
    "14848514 b.txt"
    "8504156 c.dat"
    "dir d"
    "$ cd .."])
  (process-commands
   [["$ ls"
     "dir a"
     "14848514 b.txt"
     "8504156 c.dat"
     "dir d"
     "$ cd .."] [:/] {:/ {}}]
   )
  (process-commands
   ['("$ cd ..") 
    [:/] 
    {:/ {:a {:_size 0} 
         :b.txt 14848514 
         :c.dat 8504156
         :d {:_size 0}}}]
   )
(process-commands
 [
  '() [:/] {:/ {:a {:_size 0}, :b.txt 14848514, :c.dat 8504156, :d {:_size 0}}}]
 )
  
  (let [b (solve-part1)]
    (println "result : " b))
  
  (-> (read-file-input))
  (cons [:c 2] {:a 1})
  (merge {:c 2} {:a 4})
  (dir-size
   {:/ {:a {}
        :b.txt 14848514
        :c.dat 8504156
        :d {:c.dat 8504156}}}
   )
  )