(ns advent-of-code-2023.days.day10
  (:require [clojure.string :as str]))


(def known_pipes #{"|" "-" "L" "J" "7" "F" "S"})

(defn- parse_pipe
  [x y pipe_text]
  (if (contains? known_pipes pipe_text)
    {{:x x :y y} pipe_text}
    nil))

(defn- parse_line_of_pipes
  [y text]
  (let [positions (str/split text #"")]
    (->> positions
         (map-indexed #(parse_pipe %1 y %2))
         (filter some?)
         (apply merge))))

(defn- parse_pipes
  [text]
  (let [lines (str/split text #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map-indexed parse_line_of_pipes)
         (apply merge))))

(defn- find_start
  [pipes]
  (->> pipes
       (filter (fn [[_, pipe_type]] (= pipe_type "S")))
       (first)                                              ; get the single key-value-pair
       (first)))                                            ; get the key from the kay-value-pair

(defn parse_input
  "Parses the input for day 10 of AdventOfCode 2023"
  [input_string]
  (let [pipes (parse_pipes input_string)
        start (find_start pipes)]
    {:start start :pipes pipes}))


(defn- connecting_types
  [offset]
  (case offset
    {:x 0 :y -1} #{"|" "F" "7" "S"}
    {:x 0 :y 1} #{"|" "J" "L" "S"}
    {:x -1 :y 0} #{"-" "F" "L" "S"}
    {:x 1 :y 0} #{"-" "7" "J" "S"}
    :else #{}
    ))

(defn- pipe_ends
  [pipe_type]
  (case pipe_type
    "|" #{{:x 0 :y -1} {:x 0 :y 1}}
    "-" #{{:x -1 :y 0} {:x 1 :y 0}}
    "L" #{{:x 0 :y -1} {:x 1 :y 0}}
    "J" #{{:x 0 :y -1} {:x -1 :y 0}}
    "7" #{{:x -1 :y 0} {:x 0 :y 1}}
    "F" #{{:x 1 :y 0} {:x 0 :y 1}}
    #{}
    ))

(defn- add_positions
  [pos other_pos]
  (merge-with + pos other_pos))

(defn- pipe_connections
  [position pipe_type pipes]
  (let [end_offsets (pipe_ends pipe_type)]
    (->> end_offsets
         (map (fn [end_offset] {
                                :position   (add_positions position end_offset)
                                :connecting (connecting_types end_offset)
                                }))
         (filter #(contains? (:connecting %1) (get pipes (:position %1))))
         (map :position)
         (map (fn [neighbour] {position #{neighbour} neighbour #{position}}))
         (apply (partial merge-with into)))))

(defn connections
  "The adjacent points for each point where pipe ends connect to the pipe at the point"
  [pipes]
  (->> pipes
       (map (fn [[pos pipe_type]] (pipe_connections pos pipe_type pipes)))
       (apply (partial merge-with into))))

(defn- loop_length
  [start connections]
  (let [start_direction (first (get connections start))]
    (loop [prior_pos start
           current_pos start_direction
           distance 1]
      (if (= current_pos start)
        distance
        (let [new_prior current_pos
              new_distance (inc distance)
              new_pos (->> current_pos
                           (get connections)
                           (filter #(not= %1 prior_pos))
                           (first))]
          (recur new_prior new_pos new_distance))))))

(defn- farthest_point_distance
  [{start :start pipes :pipes}]
  (let [connects (connections pipes)
        full_length (loop_length start connects)]
    (/ full_length 2)))

(defn solve_part1
  "Solves part 1 of day 10 of AdventOfCode 2023"
  [input]
  (str (farthest_point_distance input)))

(defn- loop_points
  [start connects]
  (loop [current_pos start
         in_loop #{start}]
    (let [remaining_directions (->> current_pos
                                    (get connects)
                                    (filter #(not (contains? in_loop %1))))]
      (if (empty? remaining_directions)
        in_loop
        (let [next_point (first remaining_directions)]
          (recur next_point (conj in_loop next_point)))))))

(defn- map_dimension
  [pipes]
  (->> pipes
       (keys)
       (apply (partial merge-with max))))

(defn- observation_dimensions
  [pipes]
  (let [{max_x :x max_y :y} (map_dimension pipes)]
    {:min_x -1 :max_x (inc max_x) :min_y -1 :max_y (inc max_y)}))

(def corner_types #{"F", "J", "L", "7"})


(defn- outside_corner?
  [pipe_type in_direction orientation]
  (case pipe_type
    "L" (or (and (= in_direction :horizontal) (= orientation :neg)) (and (= in_direction :vertical) (= orientation :pos)))
    "J" (or (and (= in_direction :horizontal) (= orientation :pos)) (and (= in_direction :vertical) (= orientation :neg)))
    "F" (or (and (= in_direction :horizontal) (= orientation :pos)) (and (= in_direction :vertical) (= orientation :neg)))
    "7" (or (and (= in_direction :horizontal) (= orientation :neg)) (and (= in_direction :vertical) (= orientation :pos)))))

(defn- start_in_direction
  [pipe_type orientation]
  (case pipe_type
    "J" (if (= orientation :neg) :vertical :horizontal)
    "L" (if (= orientation :neg) :horizontal :vertical)
    "7" (if (= orientation :neg) :horizontal :vertical)
    "F" (if (= orientation :neg) :vertical :horizontal)))

(defn- out_direction
  [pipe_type in_direction]
  (case pipe_type
    "7" (if (= :vertical in_direction) {:x -1 :y 0} {:x 0 :y 1})
    "F" (if (= :vertical in_direction) {:x 1 :y 0} {:x 0 :y 1})
    "L" (if (= :vertical in_direction) {:x 1 :y 0} {:x 0 :y -1})
    "J" (if (= :vertical in_direction) {:x -1 :y 0} {:x 0 :y -1})))

(defn- outside_direction
  [pipe_type in_direction]
  (case pipe_type
    "7" (if (= :horizontal in_direction) {:x 1 :y 0} {:x 0 :y -1})
    "F" (if (= :horizontal in_direction) {:x -1 :y 0} {:x 0 :y -1})
    "L" (if (= :horizontal in_direction) {:x -1 :y 0} {:x 0 :y 1})
    "J" (if (= :horizontal in_direction) {:x 1 :y 0} {:x 0 :y 1})))

(defn- other_direction
  [direction]
  (if (= direction :vertical) :horizontal :vertical))

(defn- follow_loop
  [corner outside in_loop pipes orientation]
  (let [initial_corner_type (get pipes corner)]
    (loop [pos corner
           in_direction (start_in_direction initial_corner_type orientation)
           prior_corner_type nil
           new_outside #{}]
      (let [pipe_type (get pipes pos)]
        (if (contains? corner_types pipe_type)
          (if (outside_corner? pipe_type in_direction orientation)
            (let [out_candidates (->> [:horizontal :vertical]
                       (map #(add_positions pos (outside_direction pipe_type %1)))
                       (filter #(not (contains? outside %1))))]
              (if (empty? out_candidates)
                new_outside
                (let [fresh_outside (filter #(not (contains? in_loop %1)) out_candidates)]
                  (let [new_pos (add_positions pos (out_direction pipe_type in_direction))
                        new_new_outside (into new_outside fresh_outside)]
                    (recur new_pos (other_direction in_direction) pipe_type new_new_outside)))))
            (let [new_pos (add_positions pos (out_direction pipe_type in_direction))]
              (recur new_pos (other_direction in_direction) pipe_type new_outside)))
          (let [new_pos (add_positions pos (out_direction prior_corner_type (other_direction in_direction)))]
            (recur new_pos in_direction prior_corner_type new_outside)))))))


(defn- enclosed_outside_for_corner
  [corner loop_points known_outside pipes]
  (let [orientations [:pos :neg]]
    (->> orientations
         (map #(follow_loop corner known_outside loop_points pipes %1))
         (reduce into #{}))))
(defn- find_enclosed_outside_points
  [interesting_corners loop_points known_outside pipes]
  (->> interesting_corners
       (map #(enclosed_outside_for_corner %1 loop_points known_outside pipes))
       (reduce into #{})))

(defn- in_bounds?
  [dimensions {x :x y :y}]
  (and
    (>= x (:min_x dimensions))
    (<= x (:max_x dimensions))
    (>= y (:min_y dimensions))
    (<= y (:max_y dimensions))))

(defn- outside_points
  [dimensions in_loop pipes]
  (let [start {:x (:min_x dimensions) :y (:min_y dimensions)}
        offsets #{
                  {:x 0 :y -1}
                  {:x 0 :y 1}
                  {:x -1 :y 0}
                  {:x 1 :y 0}
                  }]
    (loop [pos start
           outside #{start}
           to_inspect #{start}
           interesting_corners #{}]
      (let [neighbours (->> offsets
                            (map #(add_positions pos %1))
                            (filter #(in_bounds? dimensions %1)))
            loop_neighbours (filter #(contains? in_loop %1) neighbours)
            corner_neighbours (filter #(contains? corner_types (get pipes %1)) loop_neighbours)
            new_interesting_corners (into interesting_corners corner_neighbours)
            outside_neighbours (filter #(not (contains? in_loop %1)) neighbours)
            new_outside_neighbours (filter #(not (contains? outside %1)) outside_neighbours)
            new_outside (into outside new_outside_neighbours)
            new_to_inspect (disj (into to_inspect new_outside_neighbours) pos)]
        (if (empty? new_to_inspect)
          (let [enclosed_outside (find_enclosed_outside_points new_interesting_corners in_loop new_outside pipes)]
            (if (empty? enclosed_outside)
              new_outside
              (recur (first enclosed_outside) (into new_outside enclosed_outside) (into new_to_inspect enclosed_outside) #{})))
          (recur (first new_to_inspect) new_outside new_to_inspect new_interesting_corners))))))

(defn- start_type
  [start connections]
  (let [loop_neighbours (get connections start)]
    (if (contains? loop_neighbours (add_positions start {:x 1 :y 0}))
      (if (contains? loop_neighbours (add_positions start {:x 0 :y 1}))
        "F"
        "L")
      (if (contains? loop_neighbours (add_positions start {:x 0 :y 1}))
        "7"
        "J"))))


(defn- inside_points_count
  [start pipes]
  (let [connects (connections pipes)
        updated_pipes (assoc pipes start (start_type start connects))
        dimensions (observation_dimensions pipes)
        total_count (* (inc (- (:max_x dimensions) (:min_x dimensions))) (inc (- (:max_y dimensions) (:min_y dimensions))))
        in_loop (loop_points start connects)
        outside (outside_points dimensions in_loop updated_pipes)
        not_enclosed_count (+ (count in_loop) (count outside))]
    ;(println (result_map dimensions in_loop outside updated_pipes))
    (- total_count not_enclosed_count)))



(defn solve_part2
  "Solves part 2 of day 10 of AdventOfCode 2023"
  [input]
  (str (inside_points_count (:start input) (:pipes input))))
