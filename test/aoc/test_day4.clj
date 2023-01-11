(ns aoc.test-day4
  (:require [aoc.day4 :refer :all]
            [clojure.test :refer :all]))

(deftest test-fully-contained?
  (testing "
    Some of the pairs have noticed that one of their assignments fully contains the other. For example, 2-8 fully contains 3-7, and 6-6 is fully contained by 4-6.
            "
    (is (-> (parse-pairs "2-8,3-7")
            fully-contained?))
    (is (-> (parse-pairs "6-6,4-6")
            fully-contained?))
    (is (-> (parse-pairs "2-4,6-8")
            fully-contained?
            not))
    (is (-> (parse-pairs "2-3,4-5")
            fully-contained?
            not))
    (is (-> (parse-pairs "5-7,7-9")
            fully-contained?
            not))
    (is (-> (parse-pairs "2-6,4-8")
            fully-contained?
            not))))

(deftest test-contain-count
  (testing "In this example, there are 2 such pairs"
    (is (= (contain-count "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8") 2))))

(deftest solve-part1
  (testing "solve part1"
    (-> (slurp "./day4_input.txt")
        (contain-count))))

(deftest test-overlap?
  (testing "overlaps"
    (is (not (overlap? [[2 4] [6 8]])))
    (is (not (overlap? [[2 3] [4 5]])))
    (is (overlap? [[5 7] [7 9]]))
    (is (overlap? [[2 8] [3 7]]))
    (is (overlap? [[6 6] [4 6]]))
    (is (overlap? [[2 6] [4 8]])))
  (testing "total-overlap"
    (is (= (overlap-count "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8") 4))))

(deftest solve-part2
  (testing "solve part2"
    (is (-> (slurp "./day4_input.txt")
            overlap-count))))