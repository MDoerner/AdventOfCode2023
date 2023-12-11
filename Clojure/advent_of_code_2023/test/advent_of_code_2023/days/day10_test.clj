(ns advent-of-code-2023.days.day10-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day10 :refer [parse_input connections solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 10)
(def example_input_1 "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF\n")
(def example_input_2 "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ\n")
(def example_input_3 "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n...........\n")
(def example_input_4 "..........\n.S------7.\n.|F----7|.\n.||OOOO||.\n.||OOOO||.\n.|L-7F-J|.\n.|II||II|.\n.L--JL--J.\n..........\n")
(def example_input_5 ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...\n")
(def example_input_6 "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L\n")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input {
                          :start {:x 0 :y 2}
                          :pipes {
                                  {:x 0 :y 0} "7"
                                  {:x 1 :y 0} "-"
                                  {:x 2 :y 0} "F"
                                  {:x 3 :y 0} "7"
                                  {:x 4 :y 0} "-"
                                  {:x 1 :y 1} "F"
                                  {:x 2 :y 1} "J"
                                  {:x 3 :y 1} "|"
                                  {:x 4 :y 1} "7"
                                  {:x 0 :y 2} "S"
                                  {:x 1 :y 2} "J"
                                  {:x 2 :y 2} "L"
                                  {:x 3 :y 2} "L"
                                  {:x 4 :y 2} "7"
                                  {:x 0 :y 3} "|"
                                  {:x 1 :y 3} "F"
                                  {:x 2 :y 3} "-"
                                  {:x 3 :y 3} "-"
                                  {:x 4 :y 3} "J"
                                  {:x 0 :y 4} "L"
                                  {:x 1 :y 4} "J"
                                  {:x 3 :y 4} "L"
                                  {:x 4 :y 4} "J"
                                  }
                          }]
      (is (= parsed_input expected_input)))))

(deftest connections-test
  (testing "Test connections for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          pipes (:pipes parsed_input)
          actual_connection (connections pipes)
          expected_connections {
                                {:x 3 :y 0} #{{:x 4 :y 0}}
                                {:x 4 :y 0} #{{:x 4 :y 1} {:x 3 :y 0}}
                                {:x 0 :y 1} #{{:x 0 :y 2}}
                                {:x 1 :y 1} #{{:x 1 :y 2} {:x 2 :y 1}}
                                {:x 2 :y 1} #{{:x 1 :y 1} {:x 3 :y 1}}
                                {:x 3 :y 1} #{{:x 2 :y 1} {:x 3 :y 2}}
                                {:x 4 :y 1} #{{:x 4 :y 0} {:x 4 :y 2}}
                                {:x 0 :y 2} #{{:x 0 :y 1}}
                                {:x 1 :y 2} #{{:x 1 :y 1} {:x 1 :y 3}}
                                {:x 3 :y 2} #{{:x 3 :y 1} {:x 3 :y 3}}
                                {:x 4 :y 2} #{{:x 4 :y 1} {:x 4 :y 3}}
                                {:x 1 :y 3} #{{:x 1 :y 2} {:x 2 :y 3}}
                                {:x 2 :y 3} #{{:x 1 :y 3} {:x 3 :y 3}}
                                {:x 3 :y 3} #{{:x 2 :y 3} {:x 3 :y 2}}
                                {:x 4 :y 3} #{{:x 4 :y 2}}
                                {:x 2 :y 4} #{{:x 3 :y 4}}
                                {:x 3 :y 4} #{{:x 2 :y 4}}
                                }
          ]
      (is (= actual_connection expected_connections)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "4")))))

(deftest solve_part1-example2-test
  (testing "Test for example input 2 to part 1"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "8")))))

(deftest solve_part2-example1-test
  (testing "Test for example input 1 to part 2"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "1")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "1")))))

(deftest solve_part2-example3-test
  (testing "Test for example input 3 to part 2"
    (let [input_text example_input_3
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "4")))))

(deftest solve_part2-example4-test
  (testing "Test for example input 4 to part 2"
    (let [input_text example_input_4
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "4")))))

(deftest solve_part2-example5-test
  (testing "Test for example input 5 to part 2"
    (let [input_text example_input_5
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "8")))))

(deftest solve_part2-example6-test
  (testing "Test for example input 6 to part 2"
    (let [input_text example_input_6
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "10")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "6733"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "1016"))))

