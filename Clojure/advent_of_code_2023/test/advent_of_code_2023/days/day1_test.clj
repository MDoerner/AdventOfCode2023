(ns advent-of-code-2023.days.day1-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day1 :refer [solve_part1 solve_part2 parse_input]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 1)
(def example_input_1 "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet\n")
(def example_input_2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen\n")
(def tricky_input_line "eighthree")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)]
      (is (= parsed_input ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"])))))


(deftest solve_part1-example-test
  (testing "Test for example input to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "142")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "281")))))

(deftest solve_part2-tricky_input-test
  (testing "Test for real input to part 2"
    (let [input_text tricky_input_line
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "83")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
      (is (= (solve_problem day 1) "55123"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "55260"))))
