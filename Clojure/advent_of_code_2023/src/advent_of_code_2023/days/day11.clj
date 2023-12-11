(ns advent-of-code-2023.days.day11
  (:require [clojure.string :as str]))


(defn- parse_line_of_galaxies
  [y text]
  (let [positions (str/split text #"")]
    (->> positions
         (map-indexed #(if (= %2 "#") {:x %1 :y y} nil))
         (filter some?)
         (into #{}))))

(defn- parse_galaxies
  [text]
  (let [lines (str/split text #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map-indexed parse_line_of_galaxies)
         (reduce into #{}))))


(defn parse_input
  "Parses the input for day 11 of AdventOfCode 2023"
  [input_string]
  (parse_galaxies input_string))

(defn- galaxy_rows
  [galaxies]
  (->> galaxies
       (map :y)
       (into (sorted-set))))

(defn- galaxy_columns
  [galaxies]
  (->> galaxies
       (map :x)
       (into (sorted-set))))

(defn- empty_row_cums
  [galaxies]
  (let [rows (into [] (galaxy_rows galaxies))
        first_row (first rows)
        pairs (partition 2 1 (into [first_row] rows))
        missing_increments (map (fn [pair] [(last pair) (dec (- (last pair) (first pair)))]) pairs)
        cum_increments (reductions (fn [[_ cum] [row increment]] [row (+ increment cum)]) missing_increments)]
    (into {} cum_increments)))

(defn- empty_column_cums
  [galaxies]
  (let [cols (into []  (galaxy_columns galaxies))
        first_col (first cols)
        pairs (partition 2 1 (into [first_col] cols))
        missing_increments (map (fn [pair] [(last pair) (dec (- (last pair) (first pair)))]) pairs)
        cum_increments (reductions (fn [[_ cum] [col increment]] [col (+ increment cum)]) missing_increments)]
    (into {} cum_increments)))

(defn- galaxy_distance
  [galaxy other_galaxy col_expansion_cums row_expansion_cums multiplier]
  (let [base_row_distance (abs (- (:y other_galaxy) (:y galaxy)))
        base_col_distance (abs (- (:x other_galaxy) (:x galaxy)))
        expansion_row_distance (* (dec multiplier)
                                 (abs (- (get row_expansion_cums (:y other_galaxy)) (get row_expansion_cums (:y galaxy)))))
        expansion_col_distance (* (dec multiplier)
                                 (abs (- (get col_expansion_cums (:x other_galaxy)) (get col_expansion_cums (:x galaxy)))))]
    (reduce + [base_row_distance base_col_distance expansion_row_distance expansion_col_distance])))

(defn- pairwise_distance
  [galaxies multiplier]
  (let [col_expansion_cums (empty_column_cums galaxies)
        row_expansion_cums (empty_row_cums galaxies)
        distance_fn (fn [[x y]] (galaxy_distance x y col_expansion_cums row_expansion_cums multiplier))
        pairs (for [x galaxies y galaxies] (vector x y))
        distances (map distance_fn pairs)]
    (/ (reduce + distances) 2)))

(defn solve_part1
  "Solves part 1 of day 11 of AdventOfCode 2023"
  [input]
  (str (pairwise_distance input 2)))


(defn solve_part2
  "Solves part 2 of day 11 of AdventOfCode 2023"
  [input]
  (str (pairwise_distance input 1000000)))
