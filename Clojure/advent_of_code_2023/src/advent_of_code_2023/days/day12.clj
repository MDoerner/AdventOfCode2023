(ns advent-of-code-2023.days.day12
  (:require [clojure.string :as str]))

(defn- parse_row_map
  [text]
  (let [plots (str/split text #"")]
    (->> plots
         (map #(case %1
                 "#" true
                 "." false
                 "?" nil))
         (into []))))

(defn- parse_row_lengths
  [text]
  (->> (str/split text #",")
       (map #(Integer/parseInt %1))
       (into [])))

(defn- parse_row
  [text]
  (let [[map_text lengths_text] (str/split text #" ")]
    {:damaged? (parse_row_map map_text) :lengths (parse_row_lengths lengths_text)}))


(defn parse_input
  "Parses the input for day 12 of AdventOfCode 2023"
  [input_string]
  (let [lines (str/split input_string #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map parse_row)
         (into []))))

(defn- damage_run_possible
  [damaged length]
  (let [relevant (take (inc length) damaged)]
    (and
      (>= (count relevant) length)
      (or
        (= (count relevant) length)
        (let [after_run (last relevant)]
          (or (nil? after_run) (false? after_run))))
      (every? #(or (nil? %1) %1) (take length relevant)))))

(def possible_arrangements_count
  (memoize (fn [spring_damaged damaged_lengths]
     (loop [damaged spring_damaged
           lengths damaged_lengths]
      (if (empty? damaged)
        (if (empty? lengths) 1 0)
        (let [current_damaged (first damaged)]
          (if (false? current_damaged)
            (recur (drop 1 damaged) lengths)
            (if (empty? lengths)
              (if (nil? current_damaged) (recur (drop 1 damaged) lengths) 0)
              (let [current_length (first lengths)
                    matches? (damage_run_possible damaged current_length)]
                (if matches?
                  (if (nil? current_damaged)
                    (+
                      (possible_arrangements_count
                        (into [] (drop (inc current_length) damaged))
                        (into [] (drop 1 lengths)))
                      (possible_arrangements_count
                        (into [] (drop 1 damaged))
                        (into [] lengths)))
                    (recur (drop (inc current_length) damaged) (drop 1 lengths)))
                  (if (nil? current_damaged)
                    (recur (drop 1 damaged) lengths)
                    0                                         ;damaged, but run length does not fit
                    )))))))))))

(defn solve_part1
  "Solves part 1 of day 12 of AdventOfCode 2023"
  [input]
  (->> input
       (map (fn [{damaged :damaged? lengths :lengths}]
              (possible_arrangements_count
                (into [] damaged)
                (into [] lengths))))
       (reduce +)
       (str)))

(defn- unfold_springs
  [damaged multiplier]
  (let [with_separator (concat damaged [nil])]
    (take
      (dec (* multiplier (count with_separator)))
      (cycle with_separator))))

(defn- unfold_lengths
  [lengths multiplier]
  (take
    (* multiplier (count lengths))
    (cycle lengths)))

(defn solve_part2
  "Solves part 2 of day 12 of AdventOfCode 2023"
  [input]
  (->> input
       (map (fn [{damaged :damaged? lengths :lengths}]
              (possible_arrangements_count
                (into [] (unfold_springs damaged 5))
                (into [] (unfold_lengths lengths 5)))))
       (reduce +)
       (str)))
