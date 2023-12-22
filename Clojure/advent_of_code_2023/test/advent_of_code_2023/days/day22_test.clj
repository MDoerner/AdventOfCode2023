(ns advent-of-code-2023.days.day22-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day22 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 22)
(def example_input_1 "1,0,1~1,2,1\n0,0,2~2,0,2\n0,2,3~2,2,3\n0,0,4~0,2,4\n2,0,5~2,2,5\n0,1,6~2,1,6\n1,1,8~1,1,9\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input  [
                           [{:x 1 :y 0 :z 1} {:x 1 :y 2 :z 1}]
                           [{:x 0 :y 0 :z 2} {:x 2 :y 0 :z 2}]
                           [{:x 0 :y 2 :z 3} {:x 2 :y 2 :z 3}]
                           [{:x 0 :y 0 :z 4} {:x 0 :y 2 :z 4}]
                           [{:x 2 :y 0 :z 5} {:x 2 :y 2 :z 5}]
                           [{:x 0 :y 1 :z 6} {:x 2 :y 1 :z 6}]
                           [{:x 1 :y 1 :z 8} {:x 1 :y 1 :z 9}]
                           ]]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (time (solve_part1 parsed_input))]
      (is (= result "5")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (time (solve_part2 parsed_input))]
      (is (= result "7")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "468"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "94116351948493"))))