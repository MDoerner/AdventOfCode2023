(ns advent-of-code-2023.days.day5
  (:require [clojure.string :as str]))


(def map_header_regex #"(.+)-to-(.+) map:")

(defn- parse_seeds
  [seeds_line]
  (into [] (map #(biginteger %1) (re-seq #"\d+" seeds_line))))

(defn- parse_map_header
  [header_text]
  (let [header_match (re-find map_header_regex header_text)
        source (get header_match 1)
        target (get header_match 2)]
    {:source source :target target}))

(defn- parse_map_spec
  [spec_line]
  (let [[target source length] (into [] (map #(biginteger %1) (re-seq #"\d+" spec_line)))]
    {:target_start target :source_start source :length length}))

(defn- parse_map
  [map_part]
  (let [[header_line & spec_lines] (into [] (remove str/blank? (str/split map_part #"\r?\n")))
        header (parse_map_header header_line)
        specs (into #{} (map parse_map_spec spec_lines))]
    (assoc header :specs specs)))

(defn parse_input
  "Parses the input for day 5 of AdventOfCode 2023"
  [input_string]
  (let [[seeds_part & map_parts] (into [] (str/split input_string #"\r?\n\r?\n"))
        seeds (parse_seeds seeds_part)
        maps (map parse_map map_parts)]
    {:seeds seeds :maps maps}))

(defn- specs->fn
  [specs]
  (fn [n] (loop [remaining_specs specs]
            (if (empty? remaining_specs)
              n
              (let [[{target :target_start source :source_start length :length} & remaining] remaining_specs]
                (if (and (>= n source) (< n (+ source length)))
                  (+ target (- n source))
                  (recur remaining)))))))

(defn- mapping->fn
  [{source :source target :target specs :specs}]
  (let [map_fn (specs->fn specs)
        source_key (keyword source)
        target_key (keyword target)]
    (fn [item] (assoc item target_key (map_fn (source_key item))))))

(defn- apply_mappings
  [seed, mappings]
  (let [mapping_fns (map mapping->fn mappings)
        initial_item {:seed seed}]
    (reduce #(%2 %1) initial_item mapping_fns)))

(defn- map_seeds
  [seeds, mappings]
  (into [] (map #(apply_mappings %1 mappings) seeds)))

(defn- closest_seed_location
  [seeds, mappings]
  (let [mapped_seeds (map_seeds seeds mappings)
        locations (map :location mapped_seeds)]
    (apply min locations)))

(defn solve_part1
  "Solves part 1 of day 5 of AdventOfCode 2023"
  [input]
  (str (closest_seed_location (:seeds input) (:maps input))))

(defn- front_split_range
  [range spec]
  (let [{source :source_start} spec
        {start :start length :length} range]
    (if (and (< start source) (> (+ start length) source))
      [{:start start :length (- source start)} {:start source :length (- length (- source start))}]
      [range])))

(defn- back_split_range
  [range spec]
  (let [{source :source_start spec_length :length} spec
        {start :start length :length} range]
    (if (and (< start (+ source spec_length)) (> (+ start length) (+ source spec_length)))
      [{:start start :length (- (+ source spec_length) start)} {:start (+ source spec_length) :length (- length (- (+ source spec_length) start))}]
      [range])))

(defn- split_ranges
  [ranges spec]
  (let [front_split_ranges (flatten (map #(front_split_range %1 spec) ranges))]
    (flatten (map #(back_split_range %1 spec) front_split_ranges))))

(defn- map_ranges
  [ranges specs]
  (let [split_ranges (reduce split_ranges ranges specs)
        mapping_fn (specs->fn specs)]
    (map #(assoc %1 :start (mapping_fn (:start %1))) split_ranges)))

(defn- map_seed_ranges
  [ranges, mappings]
  (reduce #(map_ranges %1 (:specs %2)) ranges mappings))

(defn- seed_ranges
  [seeds_spec]
  (map
    (fn [spec] {:start (first spec) :length (last spec)})
    (partition 2 seeds_spec)))

  (defn- location_ranges
    [seeds_spec, mappings]
    (let [seeds (seed_ranges seeds_spec)]
      (map_seed_ranges seeds mappings)))

  (defn- closest_seed_range_location
    [seeds_spec, mappings]
    (let [locations (location_ranges seeds_spec, mappings)
          start_locations (map :start locations)]
      (apply min start_locations)))

(defn solve_part2
  "Solves part 2 of day 5 of AdventOfCode 2023"
  [input]
  (str (closest_seed_range_location (:seeds input) (:maps input))))
