(ns advent-of-code-2023.days.day3
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn- re-seq-pos [pattern string]
  (let [matcher (re-matcher pattern string)]
    ((fn step []
       (when (. matcher find)
         (cons {:start (. matcher start) :end (. matcher end) :group (. matcher group)}
               (lazy-seq (step))))))))

(def number_regex #"\d+")
(def symbol_regex #"[^0-9.]")

(defn- add_symbol_position
  [old_symbols new_position]
  (let [old_positions (get old_symbols (:symbol new_position) #{})
        new_positions (conj old_positions (:position new_position))]
    (assoc old_symbols (:symbol new_position) new_positions)))

(defn- symbols
  [y line_text]
  (let [symbol_matches (re-seq-pos symbol_regex line_text)
        converter (fn [symbol_match] {:symbol (:group symbol_match) :position {:x (:start symbol_match) :y y}})
        symbol_positions (map converter symbol_matches)]
    (reduce add_symbol_position {} symbol_positions)))

(defn- add_number_position
  [old_numbers new_position]
  (let [old_positions (get old_numbers (:number new_position) #{})
        new_positions (conj old_positions (:position new_position))]
    (assoc old_numbers (:number new_position) new_positions)))

(defn- numbers
  [y line_text]
  (let [number_matches (re-seq-pos number_regex line_text)
        converter (fn [number_match] {:number (Integer/parseInt (:group number_match)) :position {:x (:start number_match) :y y :length (- (:end number_match) (:start number_match))}})
        number_positions (map converter number_matches)
        ]
    (reduce add_number_position {} number_positions)
    ))

(defn- parse_line
  [y line_text]
  (let [line_symbols (symbols y line_text)
        line_numbers (numbers y line_text)]
    {:symbols line_symbols :numbers line_numbers}))

(defn- merge_line_items
  [line_items other_line_items]
  (merge-with
    #(merge-with into %1 %2)
    line_items
    other_line_items))

(defn parse_input
  "Parses the input for day 3 of AdventOfCode 2023"
  [input_string]
  (let [lines (remove str/blank? (str/split input_string #"\r?\n"))
        line_items (map-indexed parse_line lines)]
    (reduce merge_line_items {:symbols {} :numbers {}} line_items)))

(defn- adjacent?
  [symbol_position number_position]
  (let [{x_s :x y_s :y} symbol_position
        {x_n :x y_n :y l_n :length} number_position]
    (and
      (and (>= x_s (dec x_n)) (<= x_s (+ x_n l_n)))
      (and (>= y_s (dec y_n)) (<= y_s (inc y_n))))))

(defn- has_adjacent_symbol?
  [number_position symbols]
  (let [symbol_positions (apply set/union (vals symbols))]
    (some? (some identity (map #(adjacent? %1 number_position) symbol_positions)))))

(defn- part_numbers_positions
  [numbers symbols]
  (let [admissible? #(has_adjacent_symbol? %1 symbols)
        position_filter #(into #{} (filter admissible? %1))]
    (update-vals numbers position_filter)))

(defn- total_part_numbers
  [schematic]
  (let [{symbols :symbols numbers :numbers} schematic
        part_numbers (part_numbers_positions numbers symbols)
        number_contribution (fn [[number positions]] (* number (count positions)))]
    (reduce + (map number_contribution part_numbers))))

(defn solve_part1
  "Solves part 1 of day 3 of AdventOfCode 2023"
  [input]
  (str (total_part_numbers input)))

(defn- number_neighbours
  [position number_positions]
  (let [admissible? #(adjacent? position %1)
        position_filter #(into #{} (filter admissible? %1))
        numbers_with_neighbour_positions (update-vals number_positions position_filter)
        non-empty_value? (fn [[_, positions]] (> (count positions) 0))]
    (into {} (filter non-empty_value? numbers_with_neighbour_positions))))

(defn- neighbour_numbers
  [position number_positions]
  (let [neighbours (number_neighbours position number_positions)]
    (into [] (flatten (map (fn [[number positions]] (repeat (count positions) number)) neighbours)))))

(defn- total_gear_powers
  [symbols numbers]
  (let [potential_gears (get symbols "*")
        neighbours_per_potential_gear (map #(neighbour_numbers %1 numbers) potential_gears)
        neighbours_per_gear (filter #(= (count %1) 2) neighbours_per_potential_gear)
        gear_powers (map #(reduce * %1) neighbours_per_gear)]
    (reduce + gear_powers)))

(defn solve_part2
  "Solves part 2 of day 3 of AdventOfCode 2023"
  [input]
  (str (total_gear_powers (:symbols input) (:numbers input))))
