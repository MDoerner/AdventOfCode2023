(ns advent-of-code-2023.days.day16-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day16 :refer [parse_input solve_part1 solve_part2 energy]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 16)
(def example_input_1 ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....\n")
(def example_input_2 example_input_1)
(def example_input_3 "\\|...\\....\n|\\-.\\.....\n\\/...|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....\n")
(def example_input_4 "\\|...\\....\n.\\-.\\.....\n./../.\\...\n........|.\n...././...\n.........\\\n\\.../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....\n")
(def example_input_5 "\\|...\\....\n.\\-.\\.....\n./../.\\...\n........|.\n....-./...\n.........\\\n\\.../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....\n")
(def example_input_6 "\\|...\\....\n.\\-.\\.....\n./../.....\n........|.\n\\...|.\\...\n....\\./..\\\n\\.../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....\n")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input {
                          :width 10
                          :height 10
                          :mirrors {
                            {:x 1 :y 0} "|"
                            {:x 5 :y 0} "\\"
                            {:x 0 :y 1} "|"
                            {:x 2 :y 1} "-"
                            {:x 4 :y 1} "\\"
                            {:x 5 :y 2} "|"
                            {:x 6 :y 2} "-"
                            {:x 8 :y 3} "|"
                            {:x 9 :y 5} "\\"
                            {:x 4 :y 6} "/"
                            {:x 6 :y 6} "\\"
                            {:x 7 :y 6} "\\"
                            {:x 1 :y 7} "-"
                            {:x 3 :y 7} "-"
                            {:x 4 :y 7} "/"
                            {:x 7 :y 7} "|"
                            {:x 1 :y 8} "|"
                            {:x 6 :y 8} "-"
                            {:x 7 :y 8} "|"
                            {:x 9 :y 8} "\\"
                            {:x 2 :y 9} "/"
                            {:x 3 :y 9} "/"
                            {:x 5 :y 9} "|"
                          }
                          }]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "46")))))


(deftest solve_part1-example3-test
  (testing "Test for example input 3 to part 1"
    (let [input_text example_input_3
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "5")))))

(deftest solve_part1-example4-test
  (testing "Test for example input 4 to part 1"
    (let [input_text example_input_4
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "23")))))

(deftest solve_part1-example5-test
  (testing "Test for example input 5 to part 1"
    (let [input_text example_input_5
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "23")))))

(deftest solve_part1-example6-test
  (testing "Test for example input 6 to part 1"
    (let [input_text example_input_6
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "21")))))



(deftest energy-example1-best_start-test
  (testing "Test for example input 1 to part 1 for best start"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (energy parsed_input {:x 3 :y 0} :up)]
      (is (= result "51")))))

(deftest energy-example1-no_mirror-test
  (testing "Test for example input 1 to part 1 for best start"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (energy parsed_input {:x 0 :y 4} :left)]
      (is (= result "10")))))


(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "51")))))


(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "6361"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "6701"))))

