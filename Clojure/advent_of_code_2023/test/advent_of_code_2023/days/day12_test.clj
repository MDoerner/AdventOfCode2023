(ns advent-of-code-2023.days.day12-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day12 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 12)
(def example_input_1 "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input [
                            {:damaged? [nil nil nil false true true true] :lengths [1 1 3]}
                            {:damaged? [false nil nil false false nil nil false false false nil true true false] :lengths [1 1 3]}
                            {:damaged? [nil true nil true nil true nil true nil true nil true nil true nil] :lengths [1 3 1 6]}
                            {:damaged? [nil nil nil nil false true false false false true false false false] :lengths [4 1 1]}
                            {:damaged? [nil nil nil nil false true true true true true true false false true true true true true false] :lengths [1 6 5]}
                            {:damaged? [nil true true true nil nil nil nil nil nil nil nil] :lengths [3 2 1]}
                          ]]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "21")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "525152")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "6871"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "2043098029844"))))

