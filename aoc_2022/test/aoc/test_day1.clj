(ns aoc.test-day1
  (:require [aoc.day1 :refer :all]
            [clojure.test :refer :all]))


(deftest parse-test
  (testing "For example, suppose the Elves finish writing their items' Calories and end up with the following list:

1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"
    (is (= (parse
            "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")
           '((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000))))))

(deftest test-find-calories
  (testing "total calories"
    (is (= (find-total-calories 
            '((1000 2000 3000) 
              (4000) 
              (5000 6000) 
              (7000 8000 9000) 
              (10000)))
           24000))
    (is (= (find-three-calories
            '((1000 2000 3000)
              (4000)
              (5000 6000)
              (7000 8000 9000)
              (10000)))
           45000))))