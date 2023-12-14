(ns advent-of-code-2023.days.day14
  (:require [clojure.string :as str]))

(defn- parse_line_of_rocks
  [y text relevant_char]
  (let [positions (str/split text #"")]
    (->> positions
         (map-indexed #(if (= %2 relevant_char) {:x %1 :y y} nil))
         (filter some?)
         (into #{}))))

(defn- parse_rocks
  [lines relevant_char]
  (->> lines
       (map-indexed (fn [index line] (parse_line_of_rocks index line relevant_char)))
       (reduce into #{})))

(defn parse_input
  "Parses the input for day 14 of AdventOfCode 2023"
  [input_string]
  (let [lines (remove str/blank? (str/split input_string #"\r?\n"))]
    {
     :width (count (first lines))
     :height (count lines)
     :round (parse_rocks lines "O")
     :square (parse_rocks lines "#")
     }))

(defn- tilt_line_left
  [rounds squares]
  (loop [tilted [] rounds_index 0 squares_index 0 drop_target 0]
    (if (= rounds_index (count rounds))
      tilted
      (if (= squares_index (count squares))
        (recur (conj tilted drop_target) (inc rounds_index) squares_index (inc drop_target))
        (let [next_round (get rounds rounds_index)
              next_square (get squares squares_index)]
          (if (> next_round next_square)
            (recur tilted rounds_index (inc squares_index) (inc next_square))
            (recur (conj tilted drop_target) (inc rounds_index) squares_index (inc drop_target))))))))

(defn- reverse_line
  [line_points width]
  (into [] (reverse (map #(- (dec width) %1) line_points))))

(defn- tilt_line_right
  [rounds squares length]
  (let [rev_rounds (reverse_line rounds length)
        rev_squares (reverse_line squares length)
        rev_tilted (tilt_line_left rev_rounds rev_squares)]
    (reverse_line rev_tilted length)))

(defn- tilt_line
  [rounds squares length direction]
  (case direction
    :left (tilt_line_left rounds squares)
    :right (tilt_line_right rounds squares length)))

(defn- tilt_lines
  [rounds squares length direction]
  (->> rounds
       (map (fn [[key round]]
              (let [square (get squares key [])]
                [key (tilt_line round square length direction)])))
       (into {})))

(defn- horizontal_lines
  [points]
  (update-vals
    (reduce
      (fn [lines {x :x y :y}] (let [line (get lines y [])]
                                (assoc lines y (conj line x))))
      {}
      points)
    (comp #(into [] %1) sort))
  )

(defn- vertical_lines
  [points]
  (update-vals
    (reduce
      (fn [lines {x :x y :y}] (let [line (get lines x [])]
                                (assoc lines x (conj line y))))
      {}
      points)
    (comp #(into [] %1) sort))
  )

(defn- points_from_horizontal_lines
  [lines]
  (reduce
    (fn [points [y xs]] (into points (map (fn [x] {:x x :y y}) xs)))
    #{}
    lines))

(defn- points_from_vertical_lines
  [lines]
  (reduce
    (fn [points [x ys]] (into points (map (fn [y] {:x x :y y}) ys)))
    #{}
    lines))

(defn- tilt
  [plate direction]
  (let [rounds (case direction
                 (:north :south) (vertical_lines (:round plate))
                 (:west :east) (horizontal_lines (:round plate)))
        squares (case direction
                  (:north :south) (vertical_lines (:square plate))
                  (:west :east) (horizontal_lines (:square plate)))
        length (case direction
                 (:north :south)  (:height plate)
                 (:west :east) (:width plate))
        line_direction (case direction
                         (:north :west) :left
                         (:south :east) :right)
        new_rounds (tilt_lines rounds squares length line_direction)
        new_round (case direction
                    (:north :south) (points_from_vertical_lines new_rounds)
                    (:west :east) (points_from_horizontal_lines new_rounds))
        ]
    (assoc plate :round new_round)))

(defn- load_on_north
  [rounds height]
  (->> rounds
       (map #(- height (:y %1)))
       (reduce +)))

(defn solve_part1
  "Solves part 1 of day 14 of AdventOfCode 2023"
  [input]
  (let [tilted_plate (tilt input :north)]
  (str (load_on_north (:round tilted_plate) (:height tilted_plate)))))

(defn- cycle_once
  [plate]
  (reduce tilt plate [:north :west :south :east]))

  (defn- cycle_n_times
    [plate n]
    (loop [iteration 0 cur_plate plate encountered_iteration {}]
      (if (>= iteration n)
        cur_plate
        (if (contains? encountered_iteration cur_plate)
          (let [last_encountered (get encountered_iteration cur_plate)
                cycle_length (- iteration last_encountered)
                remaining_iterations (- n iteration)
                after_cycles (mod remaining_iterations cycle_length)]
            (cycle_n_times cur_plate after_cycles))
          (recur (inc iteration) (cycle_once cur_plate) (assoc encountered_iteration cur_plate iteration))))))

(defn solve_part2
  "Solves part 2 of day 14 of AdventOfCode 2023"
  [input]
  (let [tilted_plate (cycle_n_times input 1000000000)]
    (str (load_on_north (:round tilted_plate) (:height tilted_plate)))))
