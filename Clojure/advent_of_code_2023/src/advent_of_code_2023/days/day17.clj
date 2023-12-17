(ns advent-of-code-2023.days.day17
  (:require [clojure.string :as str])
  (:require [clojure.data.priority-map :refer [priority-map]]))


(defn- parse_line_of_heatlossmap
  [text]
  (let [positions (str/split text #"")]
    (->> positions
         (map #(Integer/parseInt %1))
         (into []))))

(defn- parse_heatlossmap
  [text]
  (let [lines (str/split text #"\r?\n")]
   (->> lines
       (remove str/blank?)
       (map parse_line_of_heatlossmap)
       (into [] merge))))

(defn parse_input
  "Parses the input for day 17 of AdventOfCode 2023"
  [input_string]
  (parse_heatlossmap input_string))

(defn- in_bounds
  [{x :x y :y} height width]
  (and (>= x 0) (>= y 0) (< x width) (< y height)))

(defn- turn_directions
  [direction]
  (case direction
    (:left :right) #{:up :down}
    (:up :down) #{:left :right}
    :none #{:left :right :up :down}))

(defn- neighbour
  [{x :x y :y} direction]
  (case direction
    :up {:x x :y (dec y)}
    :down {:x x :y (inc y)}
    :left {:x (dec x) :y y}
    :right {:x (inc x) :y y}))

(defn- neighbour_states
  [{point :location direction :in_direction length :direction_length} max_till_turn min_after_turn]
  (let [turn_neighbours (->> direction
                             (turn_directions)
                             (map (fn [dir] {
                                             :location (-> (iterate #(neighbour %1 dir) point) (nth min_after_turn))
                                             :in_direction dir
                                             :direction_length min_after_turn
                                             })))]
    (if (or (>= length max_till_turn) (= direction :none))
      turn_neighbours
      (let [straight_neighbour {
                                :location (neighbour point direction)
                                :in_direction direction
                                :direction_length (inc length)
                                }]
        (conj turn_neighbours straight_neighbour)))))

(defn- heatloss
  ([{x :x y :y} heatlossmap]
  (aget heatlossmap y x))
  ([{x :x y :y} {x_0 :x y_0 :y} heatlossmap]
   (if (= x x_0)
     (cond
       (> y y_0) (->> (range (inc y_0) (inc y))
                      (map #(heatloss {:x x :y %1} heatlossmap))
                      (reduce +))
       (< y y_0) (->> (range y y_0)
                      (map #(heatloss {:x x :y %1} heatlossmap))
                      (reduce +))
       :else 0)
     (if (= y y_0)
       (cond
         (> x x_0) (->> (range (inc x_0) (inc x))
                        (map #(heatloss {:x %1 :y y} heatlossmap))
                        (reduce +))
         (< x x_0) (->> (range x x_0)
                        (map #(heatloss {:x %1 :y y} heatlossmap))
                        (reduce +)))
       nil))))

(defn- shortest_crucible_path_length
  [heatlossmap height width start destination max_till_turn min_after_turn]
  (let [start_state {:location start :in_direction :none :direction_length 0}]
    (loop [known_distances {}
           tentative_distances (priority-map start_state 0)]
      (if (empty? heatlossmap)
        nil
        (let [[current_node current_distance] (peek tentative_distances)]
          (if (= (:location current_node) destination)
            current_distance
            (let [neighbours (->> (neighbour_states current_node max_till_turn min_after_turn)
                                  (filter #(in_bounds (:location %1) height width)))
                  new_neighbours (filter #(not (contains? known_distances %1)) neighbours)
                  with_distance (map (fn [node] [node (+ current_distance
                                                         (if (= min_after_turn (:direction_length node))
                                                          (heatloss (:location node) (:location current_node) heatlossmap)
                                                          (heatloss (:location node) heatlossmap)))]) new_neighbours)
                  better_neighbours (filter (fn [[node distance]]
                                              (or (not (contains? tentative_distances node))
                                                 (< distance (get tentative_distances node))))
                                            with_distance)
                  new_known_distances (assoc  known_distances current_node current_distance)
                  new_tentative_distances (reduce (fn [priorities [node distance]]
                                                    (assoc priorities node distance))
                                                  (pop tentative_distances)
                                                  better_neighbours)]
              (recur new_known_distances new_tentative_distances))))))))


(defn solve_part1
  "Solves part 1 of day 17 of AdventOfCode 2023"
  [input]
  (let [heatlossmap (to-array-2d input)
        height (alength heatlossmap)
        width (alength (aget heatlossmap 0))
        start {:x 0 :y 0}
        destination {:x (dec width) :y (dec height)}
        max_till_turn 3
        min_after_turn 1]
    (str (shortest_crucible_path_length heatlossmap height width start destination max_till_turn min_after_turn))))


(defn solve_part2
  "Solves part 2 of day 17 of AdventOfCode 2023"
  [input]
  (let [heatlossmap (to-array-2d input)
        height (alength heatlossmap)
        width (alength (aget heatlossmap 0))
        start {:x 0 :y 0}
        destination {:x (dec width) :y (dec height)}
        max_till_turn 10
        min_after_turn 4]
    (str (shortest_crucible_path_length heatlossmap height width start destination max_till_turn min_after_turn))))
