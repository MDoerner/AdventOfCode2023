(ns advent-of-code-2023.days.day8
  (:require [clojure.string :as str]))


(def map_node_regex #"(.+)\s+=\s+\((.+),\s+(.+)\)")

(defn- parse_directions
  [directions_text]
  (as-> directions_text arg
       (str/split arg #"")
       (replace {"R" :right "L" :left} arg)))

(defn- parse_desert_map_node
  [node_text]
  (let [node_match (re-find map_node_regex node_text)
        node_label (get node_match 1)
        left_label (get node_match 2)
        right_label (get node_match 3)]
    {node_label {:left left_label :right right_label}}))

(defn- parse_desert_map
  [map_text]
  (let [lines (str/split map_text #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map parse_desert_map_node)
         (into {}))))

(defn parse_input
  "Parses the input for day 8 of AdventOfCode 2023"
  [input_string]
  (let [[directions_part map_part] (into [] (str/split input_string #"\r?\n\r?\n"))
        directions (parse_directions directions_part)
        desert_map (parse_desert_map map_part)]
    {:directions directions :desert_map desert_map}))


(defn- move_on_map
  [map_node direction desert_map]
  (direction (get desert_map map_node)))

(defn- path_length
  [desert_map start destination_reached? directions]
  (:distance_walked
    (reduce
      (fn [{distance_so_far :distance_walked node :node} direction]
        (if (destination_reached? node)
          (reduced {:distance_walked distance_so_far :node node})
          {:distance_walked (inc distance_so_far) :node (move_on_map node direction desert_map)}))
      {:distance_walked 0 :node start}
      (cycle directions))))

(defn- single_path_length
  [desert_map start destination directions]
  (let [destination_reached? #(= destination %1)]
    (path_length desert_map start destination_reached? directions)))

(defn solve_part1
  "Solves part 1 of day 8 of AdventOfCode 2023"
  [input]
  (str (single_path_length (:desert_map input) "AAA" "ZZZ" (:directions input))))


(defn- ghost_nodes
  [desert_map indicator]
  (->> desert_map
       (keys)
       (remove #(not (str/ends-with? %1 indicator)))
       (into #{})))

(defn- gcd
  "Greatest Common Divider via Euclidean Algorithm"
  [n m]
  (if (< n m)
    (gcd m n)
    (loop [a n b m]
      (if (= b 0)
        a
        (recur b (mod a b))))))

(defn- lcm
  "Least Common Multiple via Greatest Common Divider"
  ([n m]
  (/ (* n m) (gcd n m)))
  ([numbers]
  (reduce lcm 1 numbers)))

(defn- ghost_path_length
  [desert_map start_indicator destination_indicator directions]
  (let [starts (ghost_nodes desert_map start_indicator)
        destination_reached? #(str/ends-with? %1 destination_indicator)
        individual_lengths (map #(path_length desert_map %1 destination_reached? directions) starts)]
    (lcm (map biginteger individual_lengths))))

(defn solve_part2
  "Solves part 2 of day 8 of AdventOfCode 2023"
  [input]
  (str (ghost_path_length (:desert_map input) "A" "Z" (:directions input))))
