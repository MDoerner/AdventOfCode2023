(ns advent-of-code-2023.days.day9
  (:require [clojure.string :as str]))

(defn- parse_sequence
  [text]
  (let [number_texts (str/split text #"\s+")]
    (->> number_texts
         (remove str/blank?)
         (map #(Integer/parseInt %1))
         (into []))))

(defn parse_input
  "Parses the input for day 9 of AdventOfCode 2023"
  [input_string]
  (let [lines (str/split input_string #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map parse_sequence)
         (into []))))

(def binomial
  (memoize (fn [n k]
             (cond
               (= k 0) 1
               (= n k) 1
               :else (+ (binomial (dec n) (dec k)) (binomial (dec n) k))))))

; The coefficients in the sum used to determine the next number can be transformed into the following form.
; This uses that any further differences after reaching all zeroes will also be all zeroes and not contribute to the result.
(def coefficient
  (memoize (fn [index length]
             (let [index_from_end (dec (- length index))
                   sign (if (even? index_from_end) 1 -1)
                   weight (->> (range 0 index)
                               (map #(binomial (+ %1 index_from_end) index_from_end))
                               (reduce +))]
             (* sign weight)))))

(defn- next_number
  [numbers]
  (let [length (count numbers)]
    (->> numbers
         (map-indexed #(* %2 (coefficient %1 length)))
         (reduce +))))

(defn solve_part1
  "Solves part 1 of day 9 of AdventOfCode 2023"
  [input]
  (->> input
       (map next_number)
       (reduce +)
       (str)))


(def back_coefficient
  (memoize (fn [index length]
              (let [sign (if (even? index) 1 -1)
                   weight (->> (range index length)
                               (map #(binomial %1 index))
                               (reduce +))]
               (* sign weight)))))

(defn- previous_number
  [numbers]
  (let [length (count numbers)]
    (->> numbers
         (map-indexed #(* %2 (back_coefficient %1 length)))
         (reduce +))))

(defn solve_part2
  "Solves part 2 of day 9 of AdventOfCode 2023"
  [input]
  (->> input
       (map previous_number)
       (reduce +)
       (str)))
