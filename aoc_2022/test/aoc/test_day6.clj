(ns aoc.test-day6
  (:require [aoc.day6 :refer :all]
            [clojure.test :refer :all]))

(deftest marker?-test
  (testing "afdg is a markder"
    (is (= (marker? "afdg") true)))
  (testing "afdf is not a markder"
    (is (= (marker? "afdf") false))))

(deftest find-first-marker-test
  (testing "find marker"
    (is (= (find-first-marker
            "bvwbjplbgvbhsrlpgdmjqwftvncz")
           5))
    (is (= (find-first-marker
            "nppdvjthqldpwncqszvftbrmjlhg")
           6))
    (is (= (find-first-marker
            "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
           10))
    (is (= (find-first-marker
            "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
           11))))

(deftest solve-part1
  (testing "find marker"
    (-> (slurp "./day6_input.txt")
        find-first-marker)))

(deftest test-find-first-marker-2
  (testing "find marker 2"
    (is (= (find-first-marker-2
            "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
           19))
    (is (= (find-first-marker-2
            "bvwbjplbgvbhsrlpgdmjqwftvncz")
           23))
    (is (= (find-first-marker-2
            "nppdvjthqldpwncqszvftbrmjlhg")
           23))
    (is (= (find-first-marker-2
            "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
           29))
    (is (= (find-first-marker-2
            "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
           26))))

(deftest solve-part2
  (testing "find marker"
    (-> (slurp "./day6_input.txt")
        find-first-marker-2)))