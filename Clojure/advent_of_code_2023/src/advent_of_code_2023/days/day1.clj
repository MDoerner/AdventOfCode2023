(ns advent-of-code-2023.days.day1
  (:require [clojure.string :as str]))


(defn parse_input
  "Parses the input for day 1 of AdventOfCode 2023"
  [input_string]
  (remove
    str/blank?
    (str/split input_string #"\r?\n")))

(defn- digit?
  "Determines whether a string is a single digit"
  [text]
  (not (nil? (re-find #"^\d$" text))))

(defn- find_first_digit
  "Find first digit in string"
  [text]
  (let [characters (str/split text #"")]
    (first
      (drop-while
        #(not (digit? %))
        characters))))

(defn- find_last_digit
  "Find last digit in string"
  [text]
  (let [characters (str/split text #"")]
    (first
      (drop-while
        #(not (digit? %))
        (reverse characters)))))

(defn- config_line_value
  "Finds configuration value for a line according to day 1 part 1 problem statement"
  [text]
  (let [first_digit (find_first_digit text)
        last_digit (find_last_digit text)
        result_number_text (str first_digit last_digit)]
    (Integer/parseInt result_number_text)
    ))

(defn solve_part1
  "Solves part 1 of day 1 of AdventOfCode 2023"
  [input]
  (let [config_values (map config_line_value input)
        result ( reduce + config_values)]
    (str result))
  )



(defn- text_to_digit
  "Returns the integer for a textual representation of a digit"
  [digit_text]
  (case digit_text
    ("1" "one") 1
    ("2" "two") 2
    ("3" "three") 3
    ("4" "four") 4
    ("5" "five") 5
    ("6" "six") 6
    ("7" "seven") 7
    ("8" "eight") 8
    ("9" "nine") 9
    ("0" "zero") 0))

(def digit_regex #"\d|(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)|(zero)")
(def reverse_digit_regex #"\d|(eno)|(owt)|(eerht)|(ruof)|(evif)|(xis)|(neves)|(thgie)|(enin)|(orez)")


(defn- first_digit_possibly_textual
  "Returns the first digit of a string if there is one, possibly written out"
  [text]
  (let [first_digit_match (re-find digit_regex text)
        first_digit_text (first first_digit_match)]
    (text_to_digit first_digit_text)))

(defn- last_digit_possibly_textual
  "Returns the last digit of a string if there is one, possibly written out"
  [text]
  ;; The naive way to use re-seq and to take the last does not work because re-seq does not return all matches if they overlap
  (let [reversed_text (str/reverse text)
        last_reverse_digit_match (re-find reverse_digit_regex reversed_text)
        last_reverse_digit_text (first last_reverse_digit_match)
        last_digit_text (str/reverse last_reverse_digit_text)]
    (text_to_digit last_digit_text)))

(defn- config_line_value_possibly_textual
  "Finds configuration value for a line according to day 1 part 1 problem statement"
  [text]
  (let [first_digit (first_digit_possibly_textual text)
        last_digit (last_digit_possibly_textual text)
        result_number_text (str first_digit last_digit)]
    (Integer/parseInt result_number_text)
    ))

(defn solve_part2
  "Solves part 2 of day 1 of AdventOfCode 2023"
  [input]
  (let [config_values (map config_line_value_possibly_textual input)
        result (reduce + config_values)]
    (str result))
  )
