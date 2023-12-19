(ns advent-of-code-2023.days.day18
  (:require [clojure.string :as str]))

(defn- parse_instruction
  [text]
  (let [instruction_parts (re-find #"([RLUD])\s+(\d+)\s+\((.+)\)" text)
        direction (case (get instruction_parts 1)
                    "U" :up
                    "D" :down
                    "L" :left
                    "R" :right)
        length (Integer/parseInt (get instruction_parts 2))
        color (get instruction_parts 3)]
    {:direction direction :length length :color color}))

(defn- parse_instructions
  [text]
  (let [lines (str/split text #"\r?\n")]
    (->> lines
         (remove str/blank?)
         (map parse_instruction)
         (into []))))

(defn parse_input
  "Parses the input for day 18 of AdventOfCode 2023"
  [input_string]
  (parse_instructions input_string))

(defn- trench_points
  [start_point direction length]
  (let [{x_0 :x y_0 :y} start_point]
    (case direction
      :right (map (fn [x] {:x x :y y_0}) (range x_0 (inc (+ x_0 length))))
      :left (map (fn [x] {:x x :y y_0}) (range (- x_0 length) (inc x_0)))
      :down (map (fn [y] {:x x_0 :y y}) (range y_0 (inc (+ y_0 length))))
      :up (map (fn [y] {:x x_0 :y y}) (range (- y_0 length) (inc y_0)))
    )))

(defn- end_of_trench
  [start_point direction length]
  (let [{x :x y :y} start_point]
    (case direction
      :right {:x (+ x length) :y y}
      :left {:x (- x length) :y y}
      :down {:x x :y (+ y length)}
      :up {:x x :y (- y length)}
      )))

(defn- edge_side
  [direction]
  (case direction
    (:left :right) :vertical
    (:up :down) :horizontal))



(defn- add_trench
  [[trenches end_point] instruction]
  (let [{direction :direction length :length color :color} instruction
        new_end (end_of_trench end_point direction length)
        new_points (trench_points end_point direction length)
        edge (edge_side direction)
        new_trenches (reduce #(update %1 %2 (fn [colors] (merge {edge color} colors))) trenches new_points)]
    [new_trenches new_end]))

(defn- trench_colors
  [instructions]
  (first (reduce add_trench [{} {:x 0 :y 0}] instructions)))

(defn- map_dimensions
  [trenches]
  (let [points (keys trenches)
        {max_x :x max_y :y} (apply (partial merge-with max) points)
        {min_x :x min_y :y} (apply (partial merge-with min) points)]
    {:min_x min_x :max_x max_x :min_y min_y :max_y max_y}))

(defn- observation_dimensions
  [trenches]
  (let [{max_x :max_x max_y :max_y min_x :min_x min_y :min_y} (map_dimensions trenches)]
    {:min_x (dec min_x) :max_x (inc max_x) :min_y (dec min_y) :max_y (inc max_y)}))

(defn- add_positions
  [pos other_pos]
  (merge-with + pos other_pos))

(defn- in_bounds?
  [dimensions {x :x y :y}]
  (and
    (>= x (:min_x dimensions))
    (<= x (:max_x dimensions))
    (>= y (:min_y dimensions))
    (<= y (:max_y dimensions))))

(defn- outside_points
  [dimensions trenches]
  (let [start {:x (:min_x dimensions) :y (:min_y dimensions)}
        offsets #{
                  {:x 0 :y -1}
                  {:x 0 :y 1}
                  {:x -1 :y 0}
                  {:x 1 :y 0}
                  }]
    (loop [pos start
           outside #{start}
           to_inspect #{start}]
      (let [neighbours (->> offsets
                            (map #(add_positions pos %1))
                            (filter #(in_bounds? dimensions %1)))
            outside_neighbours (filter #(not (contains? trenches %1)) neighbours)
            new_outside_neighbours (filter #(not (contains? outside %1)) outside_neighbours)
            new_outside (into outside new_outside_neighbours)
            new_to_inspect (disj (into to_inspect new_outside_neighbours) pos)]
        (if (empty? new_to_inspect)
          new_outside
          (recur (first new_to_inspect) new_outside new_to_inspect))))))

(defn- shallow_lava_lake_capacity
  [trenches]
  (let [dimensions (observation_dimensions trenches)
        total_count (* (inc (- (:max_x dimensions) (:min_x dimensions))) (inc (- (:max_y dimensions) (:min_y dimensions))))
        outside (outside_points dimensions trenches)]
    (- total_count (count outside))))

(defn solve_part1_v1
  "Solves part 1 of day 18 of AdventOfCode 2023"
  [input]
  (->> input
       (trench_colors)
       (shallow_lava_lake_capacity)
       (str)))



(defn- edges
  [instructions]
  (first (reduce (fn [[es point] {direction :direction length :length}]
            (let [end_point (end_of_trench point direction length)
                  orientation (case direction (:up :down) :vertical (:left :right) :horizontal)]
              [(-> es
                  (update point #(assoc %1 orientation end_point))
                  (update end_point #(assoc %1 orientation point))) end_point]))
          [(sorted-map-by (fn [{x_0 :x y_0 :y} {x_1 :x y_1 :y}]
                            (cond
                              (or (< x_0 x_1) (and (= x_0 x_1) (< y_0 y_1))) -1
                              (or (> x_0 x_1) (and (= x_0 x_1) (> y_0 y_1))) 1
                              :else 0)))
           {:x 0 :y 0}]
          instructions)))

(defn- enclosed_space
  [trenches]
  (loop [ts trenches area 0]
    (if (empty? ts)
      area
     (let [[top_left {top_right :horizontal bottom_left :vertical}] (first ts)
          bottom_right (:horizontal (get ts bottom_left))
          right_x (min (:x top_right) (:x bottom_right))
          left_x (:x top_left)
          top_y (:y top_left)
          bottom_y (:y bottom_left)
          intruding (->> ts
                         (keys)
                         (take-while (fn [{x :x}] (<= x right_x)))
                         (filter (fn [{y :y}] (and (< y bottom_y) (> y top_y)))))]
      (if (= bottom_right (:vertical (get ts top_right)))   ; only this square left in this component
          (let [new_area (+ area (* (inc (- right_x left_x)) (inc (- bottom_y top_y))))
                new_ts (-> ts
                           (dissoc top_left top_right bottom_left bottom_right))]
            (recur new_ts new_area))
          (let [right_border (if (empty? intruding) right_x (:x (first intruding)))
                new_top (if (= right_border (:x top_right))
                          (:vertical (get ts top_right))
                          {:x right_border :y (:y top_right)})
                new_bottom (if (= right_border (:x bottom_right))
                             (:vertical (get ts bottom_right))
                             {:x right_border :y (:y bottom_right)})
                intruders (take-while (fn [{x :x}] (<= x right_border)) intruding)
                top_adjusted_ts (if (= right_border (:x top_right))
                                    (-> ts
                                        (update new_top #(assoc %1 :vertical new_bottom))
                                        (dissoc top_right))
                                    (-> ts
                                        (assoc new_top {:vertical new_bottom :horizontal top_right})
                                        (update top_right #(assoc %1 :horizontal new_top))))
                bottom_adjusted_ts (if (= right_border (:x bottom_right))
                                    (-> top_adjusted_ts
                                        (update new_bottom #(assoc %1 :vertical new_top))
                                        (dissoc bottom_right))
                                    (-> top_adjusted_ts
                                        (assoc new_bottom {:vertical new_top :horizontal bottom_right})
                                        (update bottom_right #(assoc %1 :horizontal new_bottom))))
                front_extended_intruders (if (or (empty? intruders) (not= new_top (first intruders)))
                                                (concat [new_top] intruders)
                                                intruders)
                extended_intruders (if (= new_bottom (last front_extended_intruders))
                                          front_extended_intruders
                                          (concat front_extended_intruders [new_bottom]))
                intruder_pairs (partition 2 extended_intruders)
                intruder_adjusted_ts (reduce #(-> %1
                                         (update (first %2) (fn [neighbours] (assoc neighbours :vertical (last %2))))
                                         (update (last %2) (fn [neighbours] (assoc neighbours :vertical (first %2)))))
                                    bottom_adjusted_ts
                                    intruder_pairs)
                adjusted_ts (dissoc intruder_adjusted_ts top_left bottom_left)
                main_area (* (- right_border left_x) (inc (- bottom_y top_y)))
                top_area (if (and (not (empty? intruders)) (= new_top (first intruders)))
                           (- (:y new_top) (:y top_right))
                           0)
                bottom_area (if (and (not (empty? intruders)) (= new_bottom (last intruders)))
                           (- (:y bottom_right) (:y new_bottom ))
                           0)
                inner_intruders (filter #(and (not= %1 new_top) (not= %1 new_bottom)) intruders)
                inner_pairs (partition 2 inner_intruders)
                intruder_area (reduce + 0 (map #(dec (- (:y (last %1)) (:y (first %1)))) inner_pairs))
                new_area (+ area main_area top_area bottom_area intruder_area)]
            (recur adjusted_ts new_area)))))))


(defn solve_part1
  "Solves part 1 of day 18 of AdventOfCode 2023"
  [input]
  (->> input
       (edges)
       (enclosed_space)
       (str)))

(defn- color->instruction
  [color]
  (let [direction_char (last color)
        number_part (subs color 1 (dec (count color)))
        direction (case direction_char
                    \0 :right
                    \1 :down
                    \2 :left
                    \3 :up)
        length (BigInteger. number_part, 16)]
    {:direction direction :length length :color color}))


(defn solve_part2
  "Solves part 2 of day 18 of AdventOfCode 2023"
  [input]
  (->> input
       (map :color)
       (map color->instruction)
       (edges)
       (enclosed_space)
       (str)))
