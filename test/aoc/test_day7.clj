(ns aoc.test-day7
  (:require [aoc.day7 :refer :all]
            [clojure.test :refer :all]))

(comment 
  "test structure"
  {:/ {:a {}
       :b 3030}})

(deftest test-change-directory
  (testing "change to root"
    (is (= (process-commands 
            [["$ cd /"] [:/ :a :b] {:/ {:a {}}}])
           [[] [:/] {:/ {:a {}}}])))
  (testing "change to parent directory"
    (is (= (process-commands 
            [["$ cd .."] [:/ :a :b] {:/ {:a {}}}])
           [[] [:/ :a] {:/ {:a {}}}])))
  (testing "change to parent directory"
    (is (= (process-commands 
            [["$ cd .."] [:/] {:/ {:a {}}}])
           [[] [:/] {:/ {:a {}}}])))
  (testing "change to sub directory
            cd x moves in one level: 
            it looks in the current directory for the directory named x and 
            makes it the current directory."
    (is (= (process-commands 
            [["$ cd a"] [:/] {:/ {}}])
           [[] [:/ :a] {:/ {}}]))))

(deftest test-ls-create-file
  (testing "create directory"
    (is (= (create-file "dir a") 
           {:a {}})))
  (testing "create file"
    (is (= (create-file "14848514 b.txt")
           {:b.txt 14848514}))))

(deftest test-ls-add-file
  (testing "add directory"
    (is (= (add-file {:/ {}} [:/] {:a {}})
           {:/ {:a {}}})))
  (testing "add file"
    (is (= (add-file {:/ {}} [:/] {:b.txt 14848514})
           {:/ {:b.txt 14848514}}))))

(deftest test-list-dir
  (testing "ls"
    (is (= (list-directory
            ["$ ls"
             "dir a"
             "14848514 b.txt"
             "8504156 c.dat"
             "dir d"
             "$ cd .."] [:/] {:/ {}})
           [["$ cd .."]
            [:/]
            {:/ {:a {}
                 :b.txt 14848514
                 :c.dat 8504156
                 :d {}}}]))))

(deftest test-delete-directory
  (testing "delete directory"
    (is (= (delete-directory
            [:/ :a]
            {:/ {:a {}
                 :b.txt 14848514
                 :c.dat 8504156
                 :d {}}})
           {:/ {:b.txt 14848514
                :c.dat 8504156
                :d {}}}))))

(deftest test-read-file-input
  (test "solve day7-input"))
     
     
     
    
    
  




(comment 
  (get  {:/ 10} :/))
  