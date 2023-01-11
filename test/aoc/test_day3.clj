(ns aoc.test-day3
  (:require [aoc.day3 :refer :all]
            [clojure.test :refer :all]))

(deftest selected-point
  (testing "The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which means its first compartment contains the items vJrwpWtwJgWr, while the second compartment contains the items hcsFMMfFFhFp. The only item type that appears in both compartments is lowercase p."
    (is (= (common-type "vJrwpWtwJgWrhcsFMMfFFhFp")
           #{\p})))
  (testing "The second rucksack's compartments contain jqHRNqRjqzjGDLGL and rsFMfFZSrLrFZsSL. The only item type that appears in both compartments is uppercase L."
    (is (= (common-type
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")) #{\L}))
  (testing "The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only common item type is uppercase P."
    (is (= (common-type
            "PmmdzqPrVvPwwTWBwg")
           #{\P})))
  (testing "The fourth rucksack's compartments only share item type v."
    (is (= (common-type
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")
           #{\v})))
  (testing "The fifth rucksack's compartments only share item type t."
    (is (= (common-type
            "ttgJtRGJQctTZtZT")
           #{\t})))

  (testing "The sixth rucksack's compartments only share item type s."
    (is (= (common-type
            "CrZsJsPPZsGzwwsLwLmpwMDw")
           #{\s}))))

(deftest test-priority
  (testing "Lowercase item types a through z have priorities 1 through 26."
    (is (->> (range (int \a) (inc (int \z)))
             (map #(-> % char unit-priority))
             (= (range 1 (inc 26))))))
  (testing "Uppercase item types A through Z have priorities 27 through 52."
    (is (->> (range (int \A) (inc (int \Z)))
             (map #(-> % char unit-priority))
             (= (range 27 (inc 52)))))))

(deftest some-priority
  (testing "In the above example, the priority of the item type that appears in both compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157."
    (is (= (total-priority "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")
           157))))

(deftest solve-problem
  (testing "solve problem"
    (-> (slurp "./day3_input.txt")
        total-priority)))

(deftest three-elf-group
  (testing "In the first group, the only item type that appears in all three rucksacks is lowercase r;"
    (is (= (group-item-type '("vJrwpWtwJgWrhcsFMMfFFhFp"
                              "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                              "PmmdzqPrVvPwwTWBwg")) #{\r})))
  (testing "In the second group, their badge item type must be Z."
    (is (= (group-item-type
            '("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" "ttgJtRGJQctTZtZT" "CrZsJsPPZsGzwwsLwLmpwMDw"))
           #{\Z})))
  (testing "sum of priority group"
    (is (= (total-group-item-type
            "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")
           70))))

(deftest solve-problem-2
  (testing "solve problem"
    (-> (slurp "./day3_input.txt")
        (total-group-item-type))))

