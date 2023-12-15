(ns advent-of-code-2023.days.day15-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day15 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 15)
(def example_input_1 "HASH\n")
(def example_input_2 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n")

(def example_list ["rn=1" "cm-" "qp=3" "cm=2" "qp-" "pc=4" "ot=9" "ab=5" "pc-" "pc=6" "ot=7"])

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input ["rn=1" "cm-" "qp=3" "cm=2" "qp-" "pc=4" "ot=9" "ab=5" "pc-" "pc=6" "ot=7"]]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "52")))))

(deftest solve_part1-example2-test
  (testing "Test for example input 2 to part 1"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "1320")))))

(deftest solve_part1-example_list-test
  (testing "Test for example input 2 to part 1"
    (let [input_texts example_list
          parsed_inputs (map parse_input input_texts)
          results (into [] (map solve_part1 parsed_inputs))
          expecteds ["30" "253" "97" "47" "14" "180" "9" "197" "48" "214" "231"]]
      (is (= results expecteds)))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "145")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "507769"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "269747"))))

