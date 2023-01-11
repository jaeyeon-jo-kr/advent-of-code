(ns aoc.test-day8
  (:require [aoc.day8 :refer :all]
            [clojure.test :refer :all]))

(deftest test-parse-tree
  (testing "parse tree
   30373
   25512
   65332
   33549
   35390"
    (is 
     (= (parse-map
          (str "30373\n"
               "25512\n"
               "65332\n" 
               "33549\n" 
               "35390"))
           [[3 0 3 7 3]
            [2 5 5 1 2]
            [6 5 3 3 2]
            [3 3 5 4 9]
            [3 5 3 9 0]]))))



(deftest test-isvisible
  (testing "is visible or not"
    (is (visible? tree-map 0 0))
    (is (visible? tree-map 1 0))
    (is (visible? tree-map 2 0))
    (is (visible? tree-map 3 0))
    (is (visible? tree-map 4 0))
    (is (visible? tree-map 1 4))
    (is (visible? tree-map 2 4))
    (is (visible? tree-map 3 4))
    (is (visible? tree-map 4 4))
    (is (visible? tree-map 0 1))
    (is (visible? tree-map 0 2))
    (is (visible? tree-map 0 3))
    (is (visible? tree-map 0 4))
    (is (visible? tree-map 4 1))
    (is (visible? tree-map 4 2))
    (is (visible? tree-map 4 3)))
  (testing "The top-left 5 is visible from the left and top.
            (It isn't visible from the right or bottom 
            since other trees of height 5 are in the way.)"
    (is (visible? tree-map 1 1)))
  (testing "The top-middle 5 is visible from the top and right."
    (is (visible? tree-map 1 2))
    )
  (testing "The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge."
    (is (not (visible? tree-map 1 3))))
  
  (testing "The left-middle 5 is visible, but only from the right."
    (is (visible? tree-map 2 1)))
  
  (testing "The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge."
    (is (not (visible? tree-map 2 2))))
  
  (testing "The right-middle 3 is visible from the right."
    (is (visible? tree-map 2 3)))
  
  (testing "In the bottom row, the middle 5 is visible, but the 3 and 4 are not."
    (is (visible? tree-map 3 2))
    (is (not (visible? tree-map 3 1)))
    (is (not (visible? tree-map 3 3)))
    )
  
  )

(deftest test-day8
  (-> (slurp "./day8_input.txt")
      parse-map
      visible-count))

(deftest test-seeing-from-tree
  (testing "seeing from tree
            Looking up, its view is not blocked; it can see 1 tree (of height 3)."
    (is (= (tree-view-count 5 '(3))
           1))
    )
  (testing "Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it)."
    (is (= (tree-view-count 5 '(5 2)) 
           1)))
  
  (testing "Looking right, its view is not blocked; it can see 2 trees."
    (is (= (tree-view-count 5 '(1 2)) 
           2)))
  
  (testing "Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that blocks its view)."
    (is (= (tree-view-count 5 '(3 5 3)) 
           2)))
  (testing "
Looking left, its view is not blocked; it can see 2 trees."
    (is (= (tree-view-count 5 '(3 3))
           2)))
  (testing "Looking right, its view is blocked at 2 trees (by a massive tree of height 9)."
    (is (= (tree-view-count 5 '(4 9))
           2))
    )
  )

(deftest test-day8-part-2
  (-> (slurp "./day8_input.txt")
      parse-map
      max-scening-score))
