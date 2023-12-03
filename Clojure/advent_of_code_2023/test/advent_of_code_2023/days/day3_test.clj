(ns advent-of-code-2023.days.day3-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day3 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 3)
(def example_input_1 "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input {
                          :symbols {
                                    "*" #{{:x 3 :y 1} {:x 3 :y 4} {:x 5 :y 8}}
                                    "$" #{{:x 3 :y 8}}
                                    "#" #{{:x 6 :y 3}}
                                    "+" #{{:x 5 :y 5}}
                                    }
                          :numbers {
                                    467 #{{:x 0 :y 0 :length 3}}
                                    114 #{{:x 5 :y 0 :length 3}}
                                    35 #{{:x 2 :y 2 :length 2}}
                                    633 #{{:x 6 :y 2 :length 3}}
                                    617 #{{:x 0 :y 4 :length 3}}
                                    58 #{{:x 7 :y 5 :length 2}}
                                    592 #{{:x 2 :y 6 :length 3}}
                                    755 #{{:x 6 :y 7 :length 3}}
                                    664 #{{:x 1 :y 9 :length 3}}
                                    598 #{{:x 5 :y 9 :length 3}}
                                    }
                          }]
      (is (= parsed_input expected_input)))))


(deftest solve_part1-example-test
  (testing "Test for example input to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "4361")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "467835")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "553825"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "93994191"))))
