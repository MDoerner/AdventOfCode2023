(ns advent-of-code-2023.days.day19-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2023.days.day19 :refer [parse_input solve_part1 solve_part2]])
  (:require [advent-of-code-2023.core :refer [solve_problem]]))

(def day 19)
(def example_input_1 "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}\n")
(def example_input_2 example_input_1)
(def example_input_3 "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}")
(def example_input_4 "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=1679,m=44,a=2067,s=496}")
(def example_input_5 "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=2036,m=264,a=79,s=2244}")
(def example_input_6 "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=2461,m=1339,a=466,s=291}")
(def example_input_7 "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=2127,m=1623,a=2188,s=1013}")

(deftest parse_input-test
  (testing "Test parsing for example input"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          expected_input  { :workflows [
                                        {:id "px"
                                         :tests [
                                                {:operator "<" :variable :a :value 2006 :result "qkq"}
                                                {:operator ">" :variable :m :value 2090 :result :accepted}
                                                ]
                                         :default "rfg"
                                         }
                                        {:id "pv"
                                         :tests [{:operator ">" :variable :a :value 1716 :result :rejected}]
                                         :default :accepted
                                         }
                                        {:id "lnx"
                                         :tests [{:operator ">" :variable :m :value 1548 :result :accepted}]
                                         :default :accepted
                                         }
                                        {:id "rfg"
                                         :tests [
                                                 {:operator "<" :variable :s :value 537 :result "gd"}
                                                 {:operator ">" :variable :x :value 2440 :result :rejected}
                                                 ]
                                         :default :accepted
                                         }
                                        {:id "qs"
                                         :tests [{:operator ">" :variable :s :value 3448 :result :accepted}]
                                         :default "lnx"
                                         }
                                        {:id "qkq"
                                         :tests [{:operator "<" :variable :x :value 1416 :result :accepted}]
                                         :default "crn"
                                         }
                                        {:id "crn"
                                         :tests [{:operator ">" :variable :x :value 2662 :result :accepted}]
                                         :default :rejected
                                         }
                                        {:id "in"
                                         :tests [{:operator "<" :variable :s :value 1351 :result "px"}]
                                         :default "qqz"
                                         }
                                        {:id "qqz"
                                         :tests [
                                                 {:operator ">" :variable :s :value 2770 :result "qs"}
                                                 {:operator "<" :variable :m :value 1801 :result "hdj"}
                                                 ]
                                         :default :rejected
                                         }
                                        {:id "gd"
                                         :tests [{:operator ">" :variable :a :value 3333 :result :rejected}]
                                         :default :rejected
                                         }
                                        {:id "hdj"
                                         :tests [{:operator ">" :variable :m :value 838 :result :accepted}]
                                         :default "pv"
                                         }
                                        ]
                           :parts [
                                   {:x 787 :m 2655 :a 1222 :s 2876}
                                   {:x 1679 :m 44 :a 2067 :s 496}
                                   {:x 2036 :m 264 :a 79 :s 2244}
                                   {:x 2461 :m 1339 :a 466 :s 291}
                                   {:x 2127 :m 1623 :a 2188 :s 1013}
                                   ]}]
      (is (= parsed_input expected_input)))))

(deftest solve_part1-example1-test
  (testing "Test for example input 1 to part 1"
    (let [input_text example_input_1
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "19114")))))

(deftest solve_part1-example3-test
  (testing "Test for example input 3 to part 1"
    (let [input_text example_input_3
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "7540")))))

(deftest solve_part1-example4-test
  (testing "Test for example input 4 to part 1"
    (let [input_text example_input_4
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "0")))))

(deftest solve_part1-example5-test
  (testing "Test for example input 5 to part 1"
    (let [input_text example_input_5
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "4623")))))

(deftest solve_part1-example6-test
  (testing "Test for example input 6 to part 1"
    (let [input_text example_input_6
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "0")))))

(deftest solve_part1-example7-test
  (testing "Test for example input 7 to part 1"
    (let [input_text example_input_7
          parsed_input (parse_input input_text)
          result (solve_part1 parsed_input)]
      (is (= result "6951")))))

(deftest solve_part2-example2-test
  (testing "Test for example input 2 to part 2"
    (let [input_text example_input_2
          parsed_input (parse_input input_text)
          result (solve_part2 parsed_input)]
      (is (= result "167409079868000")))))

(deftest solve_part1-input-test
  (testing "Test for real input to part 1"
    (is (= (solve_problem day 1) "263678"))))


(deftest solve_part2-input-test
  (testing "Test for real input to part 2"
    (is (= (solve_problem day 2) "125455345557345"))))

