(ns advent-of-code-2023.days.day6
  (:require [clojure.string :as str])
  (:require [clojure.math :as math]))


(defn- parse_line
  [line_text]
  (map biginteger (re-seq #"\d+" line_text)))


(defn parse_input
  "Parses the input for day 6 of AdventOfCode 2023"
  [input_string]
  (let [lines (remove str/blank? (str/split input_string #"\r?\n"))
        numbers (map parse_line lines)
        pairs (map (fn [t d] {:time t :distance d}) (first numbers) (nth numbers 1))]
    (into [] pairs)))


(defn- better_than_record_range
  [{time :time distance :distance}]
  (let [half_time (/ time 2)
        root_part (math/sqrt (- (* half_time half_time) distance))
        min_candidate (biginteger (math/ceil (- half_time root_part)))
        min_time (if (> (* min_candidate (- time min_candidate)) distance) min_candidate (inc min_candidate))
        max_candidate (biginteger (math/ceil (+ half_time root_part)))
        max_time (if (> (* max_candidate (- time max_candidate)) distance) max_candidate (dec max_candidate))]
    {:min min_time :max max_time}))

(defn- range_length
  [{min :min max :max}]
  (inc (- max min)))

(defn- margin_of_error
  [race]
  (range_length (better_than_record_range race)))

(defn solve_part1
  "Solves part 1 of day 6 of AdventOfCode 2023"
  [input]
  (str (reduce * (map margin_of_error input))))


(defn- merge_races
  [{t_o :time d_o :distance} {t_n :time d_n :distance}]
  {:time (biginteger (str t_o t_n)) :distance (biginteger (str d_o d_n))})

(defn solve_part2
  "Solves part 2 of day 6 of AdventOfCode 2023"
  [input]
  (str (margin_of_error (reduce merge_races input))))
