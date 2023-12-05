(ns advent-of-code-2023.days.day5-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day5 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 5)
(def example_input_1 "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4\n")
(def example_input_2 example_input_1)

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          expected_input {
                          :seeds [79 14 55 13]
                          :maps [
                                 {
                                  :source "seed"
                                  :target "soil"
                                  :specs #{
                                           {:target_start 50 :source_start 98 :length 2}
                                           {:target_start 52 :source_start 50 :length 48}
                                          }
                                 }
                                 {
                                  :source "soil"
                                  :target "fertilizer"
                                  :specs #{
                                           {:target_start 0 :source_start 15 :length 37}
                                           {:target_start 37 :source_start 52 :length 2}
                                           {:target_start 39 :source_start 0 :length 15}
                                          }
                                 }
                                 {
                                  :source "fertilizer"
                                  :target "water"
                                  :specs #{
                                           {:target_start 49 :source_start 53 :length 8}
                                           {:target_start 0 :source_start 11 :length 42}
                                           {:target_start 42 :source_start 0 :length 7}
                                           {:target_start 57 :source_start 7 :length 4}
                                           }
                                  }
                                 {
                                  :source "water"
                                  :target "light"
                                  :specs #{
                                           {:target_start 88 :source_start 18 :length 7}
                                           {:target_start 18 :source_start 25 :length 70}
                                           }
                                  }
                                 {
                                  :source "light"
                                  :target "temperature"
                                  :specs #{
                                           {:target_start 45 :source_start 77 :length 23}
                                           {:target_start 81 :source_start 45 :length 19}
                                           {:target_start 68 :source_start 64 :length 13}
                                           }
                                  }
                                 {
                                  :source "temperature"
                                  :target "humidity"
                                  :specs #{
                                           {:target_start 0 :source_start 69 :length 1}
                                           {:target_start 1 :source_start 0 :length 69}
                                           }
                                  }
                                 {
                                  :source "humidity"
                                  :target "location"
                                  :specs #{
                                           {:target_start 60 :source_start 56 :length 37}
                                           {:target_start 56 :source_start 93 :length 4}
                                           }
                                  }
                                 ]
                          }]
      (is (= parsed_input expected_input)))))


(deftest solve_part1-example-test
  (testing "Test for example input to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "650599855")))))

(deftest solve_part2-example-test
  (testing "Test for example input to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "46")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "8063216"))))

(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "1240035"))))

