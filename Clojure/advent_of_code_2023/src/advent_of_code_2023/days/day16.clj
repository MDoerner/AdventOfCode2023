(ns advent-of-code-2023.days.day16
  (:require [clojure.string :as str]))


(defn- parse_line_of_mirrors
  [y text]
  (let [positions (str/split text #"")]
    (->> positions
         (map-indexed #(if (contains? #{"|", "-", "\\", "/"} %2) [{:x %1 :y y} %2] nil))
         (filter some?)
         (into {}))))

(defn- parse_mirrors
  [lines]
  (->> lines
       (map-indexed parse_line_of_mirrors)
       (apply merge)))

(defn parse_input
  "Parses the input for day 16 of AdventOfCode 2023"
  [input_string]
  (let [lines (remove str/blank? (str/split input_string #"\r?\n"))]
    {
     :width (count (first lines))
     :height (count lines)
     :mirrors (parse_mirrors lines)
     }))

(defn- mirrors_by_column
  [mirrors]
  (->> mirrors
       (keys)
       (map (fn [{x :x y :y}] {x (sorted-set y)}))
       (reduce (partial merge-with into))))

(defn- mirrors_by_row
  [mirrors]
  (->> mirrors
       (keys)
       (map (fn [{x :x y :y}] {y (sorted-set x)}))
       (reduce (partial merge-with into))))

(defn- out_ray_directions
  [mirror_type side]
  (case mirror_type
    "|" #{:up :down}
    "-" #{:left :right}
    "\\" (if (= side :left) #{:left :down} #{:right :up})
    "/" (if (= side :left) #{:left :up} #{:right :down})))

(defn- mirror_side
  [type from_direction]
  (case type
    "\\" (if (contains? #{:left :down} from_direction) :left :right)
    "/" (if (contains? #{:left :up} from_direction) :left :right)
    :none))

(defn- end_of_ray
  [start direction mirrors_in_row mirrors_in_column mirrors height width]
  (case direction
    :down (let [{x :x y :y} start
                lower_mirror_ys (drop-while #(<= %1 y) (get mirrors_in_column x))]
            (if (empty? lower_mirror_ys)
              {:location {:x x :y (dec height)} :type :edge :side :none}
              (let [new_y (first lower_mirror_ys)
                    new_location {:x x :y new_y}
                    new_type (get mirrors new_location)
                    side (mirror_side new_type :up)]
                {:location new_location :type new_type :side side})))
    :up (let [{x :x y :y} start
                higher_mirror_ys (drop-while #(>= %1 y) (reverse (get mirrors_in_column x)))]
            (if (empty? higher_mirror_ys)
              {:location {:x x :y 0} :type :edge :side :none}
              (let [new_y (first higher_mirror_ys)
                    new_location {:x x :y new_y}
                    new_type (get mirrors new_location)
                    side (mirror_side new_type :down)]
                {:location new_location :type new_type :side side})))
    :left (let [{x :x y :y} start
                lower_mirror_xs (drop-while #(>= %1 x) (reverse (get mirrors_in_row y)))]
            (if (empty? lower_mirror_xs)
              {:location {:x 0 :y y} :type :edge :side :none}
              (let [new_x (first lower_mirror_xs)
                    new_location {:x new_x :y y}
                    new_type (get mirrors new_location)
                    side (mirror_side new_type :right)]
                {:location new_location :type new_type :side side})))
    :right (let [{x :x y :y} start
                higher_mirror_xs (drop-while #(<= %1 x) (get mirrors_in_row y))]
            (if (empty? higher_mirror_xs)
              {:location {:x (dec width) :y y} :type :edge :side :none}
              (let [new_x (first higher_mirror_xs)
                    new_location {:x new_x :y y}
                    new_type (get mirrors new_location)
                    side (mirror_side new_type :left)]
                {:location new_location :type new_type :side side})))))

(defn- light_rays
  [mirrors width height start_point edge_side]
  (let [by_row (mirrors_by_row mirrors)
        by_column (mirrors_by_column mirrors)
        {start_x :x start_y :y} start_point]
    (cond
      (and (contains? #{:left :right} edge_side) (not (contains? by_row start_y)))
      #{#{{:location {:x 0 :y start_y} :type :edge :side :none} {:location {:x (dec width) :y start_y} :type :edge :side :none}}}
      (and (contains? #{:up :down} edge_side) (not (contains? by_column start_x)))
      #{#{{:location {:x start_x :y 0} :type :edge :side :none} {:location {:x start_x :y (dec height)} :type :edge :side :none}}}
      :else (let [start_mirror_x (case edge_side
                                   :left (first (get by_row start_y))
                                   :right (last (get by_row start_y))
                                   (:up :down) start_x)
                  start_mirror_y (case edge_side
                                   :up (first (get by_column start_x))
                                   :down (last (get by_column start_x))
                                   (:left :right) start_y)
                  start_mirror_location {:x start_mirror_x :y start_mirror_y}
                  start_mirror_type (get mirrors start_mirror_location)
                  start_mirror {:location start_mirror_location :type start_mirror_type :side (mirror_side start_mirror_type edge_side)}
                  start_ray #{{:location start_point :type :edge :side :none} start_mirror}]
              (loop [rays #{start_ray} visited_mirrors #{} new_light_sources #{start_mirror}]
                (if (empty? new_light_sources)
                  rays
                  (let [source_mirror (first new_light_sources)
                        {location :location type :type side :side} source_mirror
                        target_directions (out_ray_directions type side)
                        new_targets_with_direction (map (fn [dir] {:direction dir :end (end_of_ray location dir by_row by_column mirrors width height)}) target_directions)
                        relevant_new_targets_with_direction (filter (fn [{dir :direction end :end}]
                                                                      (or
                                                                        (not (contains? visited_mirrors end))
                                                                        (and (= "|" (:type end)) (contains? #{:left :right} dir))
                                                                        (and (= "-" (:type end)) (contains? #{:up :down} dir))))
                                                                    new_targets_with_direction)
                        relevant_new_targets (map :end relevant_new_targets_with_direction)
                        new_rays (map (fn [end_point] #{source_mirror end_point}) relevant_new_targets)
                        new_sources (filter #(and (not= :edge (:type %1)) (not (contains? visited_mirrors %1))) relevant_new_targets)]
                    ;(println " ")
                    ;(println source_mirror)
                    ;(println relevant_new_targets)
                    ;(println new_rays)
                    ;(println new_sources)
                    ;(println " ")
                    (recur (into rays new_rays) (conj visited_mirrors source_mirror) (into (disj new_light_sources source_mirror) new_sources)))))))))

(defn- mirror_hits
  [rays]
  (let [ends (reduce into [] rays)
        mirror_ends (filter #(not= :edge (:type %1)) ends)
        end_points (map :location mirror_ends)]
    (-> (group-by identity end_points)
        (update-vals count))))

(defn- horizontal?
  [ray]
  (let [start (first ray)
        end (last ray)]
    (= (:y (:location start)) (:y (:location end)))))

(defn- horizontal_ray_extent
  [ray]
  (let [{loc_1 :location type_1 :type} (first ray)
        {x_1 :x y_1 :y} loc_1
        {loc_2 :location type_2 :type} (last ray)
        x_2 (:x loc_2)
        min_x (min (if (= type_1 :edge) (dec x_1) x_1) (if (= type_2 :edge) (dec x_2) x_2))
        max_x (max (if (= type_1 :edge) (inc x_1) x_1) (if (= type_2 :edge) (inc x_2) x_2))]
    {:y y_1 :min_x min_x :max_x max_x}))

(defn- vertical_ray_extent
  [ray]
  (let [{loc_1 :location type_1 :type} (first ray)
        {x_1 :x y_1 :y} loc_1
        {loc_2 :location type_2 :type} (last ray)
        y_2 (:y loc_2)
        min_y (min (if (= type_1 :edge) (dec y_1) y_1) (if (= type_2 :edge) (dec y_2) y_2))
        max_y (max (if (= type_1 :edge) (inc y_1) y_1) (if (= type_2 :edge) (inc y_2) y_2))]
    {:x x_1 :min_y min_y :max_y max_y}))

(defn- intersecting?
  [horizontal_extent vertical_extent]
  (let [{y :y min_x :min_x max_x :max_x} horizontal_extent
        {x :x min_y :min_y max_y :max_y} vertical_extent]
    (and (< y max_y) (> y min_y) (< x max_x) (> x min_x))))

(defn- intersections_count
  [rays]
  (let [horizontal_rays (filter horizontal? rays)
        vertical_rays (filter #(not (horizontal? %1)) rays)
        horizontal_extents (map horizontal_ray_extent horizontal_rays)
        vertical_extents (map vertical_ray_extent vertical_rays)
        combinations (for [hor horizontal_extents vert vertical_extents] (vector hor vert))
        intersecting (filter (fn [[hor vert]] (intersecting? hor vert)) combinations)]
    (count intersecting)))

(defn- ray_length
  [ray]
  (let [{loc_1 :location} (first ray)
        {x_1 :x y_1 :y} loc_1
        {loc_2 :location} (last ray)
        {x_2 :x y_2 :y} loc_2]
    (if (= x_1 x_2)
      (inc (abs (- y_2 y_1)))
      (inc (abs (- x_2 x_1))))))

(defn- energized_count
  [mirrors width height start_point edge_side]
  (let [rays (light_rays mirrors width height start_point edge_side)
        total_ray_length (reduce + (map ray_length rays))
        mirror_visits (mirror_hits rays)
        total_mirror_visits (reduce + (vals mirror_visits))
        visited_mirrors (count (keys mirror_visits))
        intersection_count (intersections_count rays)
        ]
    ;(println total_ray_length)
    ;(println total_mirror_visits)
    ;(println visited_mirrors)
    ;(println intersection_count)
    ;(doseq [r rays] (println r))
    (- (- total_ray_length intersection_count) (- total_mirror_visits visited_mirrors))))


(defn energy
  [input start_point edge_side]
  (let [{mirrors :mirrors width :width height :height} input]
    (str (energized_count mirrors width height start_point edge_side))))

(defn solve_part1
  "Solves part 1 of day 16 of AdventOfCode 2023"
  [input]
  (str (energy input {:x 0 :y 0} :left)))

(defn- entry_points
  [width height]
  (let [left_edge (map (fn [y] {:location {:x 0 :y y} :edge_side :left}) (range 0 height))
        last_x (dec width)
        right_edge (map (fn [y] {:location {:x last_x :y y} :edge_side :right}) (range 0 width))
        upper_edge (map (fn [x] {:location {:x x :y 0} :edge_side :up}) (range 0 height))
        last_y (dec height)
        lower_edge (map (fn [x] {:location {:x x :y last_y} :edge_side :down}) (range 0 width))]
    (concat left_edge right_edge upper_edge lower_edge)))


(defn max_energy
  [mirrors width height]
  (let [starts (entry_points width height)
        energies (map #(energized_count mirrors width height (:location %1) (:edge_side %1)) starts)]
    (apply max energies)))

(defn solve_part2
  "Solves part 2 of day 16 of AdventOfCode 2023"
  [input]
  (let [{mirrors :mirrors width :width height :height} input]
    (str (max_energy mirrors width height))))
