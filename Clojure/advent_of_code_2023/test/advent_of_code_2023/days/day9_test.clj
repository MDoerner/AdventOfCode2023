(ns advent-of-code-2023.days.day9-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day9 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 9)
(def example_input_1 "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input [
                          [0 3 6 9 12 15]
                          [1 3 6 10 15 21]
                          [10 13 16 21 30 45]
                          ]]
      (is (= parsed_input expected_input)))))


(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "114")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "2")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "2098530125"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "1016"))))

