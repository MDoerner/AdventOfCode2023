(ns advent-of-code-2023.days.day24-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day24 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 24)
(def example_input_1 "19, 13, 30 @ -2,  1, -2\n18, 19, 22 @ -1, -1, -2\n20, 25, 34 @ -2, -2, -4\n12, 31, 28 @ -1, -2, -1\n20, 19, 15 @  1, -5, -3\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input  {
                           :min 200000000000000
                           :max 400000000000000
                           :hail-drops [
                           {:position {:x 19 :y 13 :z 30} :velocity {:x -2 :y 1 :z -2}}
                           {:position {:x 18 :y 19 :z 22} :velocity {:x -1 :y -1 :z -2}}
                           {:position {:x 20 :y 25 :z 34} :velocity {:x -2 :y -2 :z -4}}
                           {:position {:x 12 :y 31 :z 28} :velocity {:x -1 :y -2 :z -1}}
                           {:position {:x 20 :y 19 :z 15} :velocity {:x 1 :y -5 :z -3}}
                           ]}]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          adjusted_input (assoc (assoc parsed_input :min 7) :max 27)
          result (time (solve_part1 adjusted_input))]
      (is (= result "2")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (time (parse_input input_text))
          result (solve_part2 parsed_input)]
      (is (= result "952408144115")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "16727"))))


(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "94116351948493"))))