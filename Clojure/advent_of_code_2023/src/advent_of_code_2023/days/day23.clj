(ns advent-of-code-2023.days.day23
  (:require [clojure.string :as str]))

(defn parse_input
  "Parses the input for day 23 of AdventOfCode 2023"
  [input_string]
  (let [lines (remove str/blank? (str/split input_string #"\r?\n"))
        height (count lines)
        width (count (first lines))
        start {:x 1 :y 0}
        destination {:x (- width 2) :y (dec height)}
        trail_map (->> lines
                       (map #(into [] (str/split %1 #"")))
                       (into []))]
    {
     :start start
     :destination destination
     :trail_map trail_map
     }
    ))



; The input has the very special property that each trail between forks, joins and intersections, starts with a slope and ends with one.
; This allows to condense the trails to a comparatively small graph with the above trails being the edges.
; If the property is not satisfied, the behaviour of the below code is undefined.

(defn- add_positions
  [pos other_pos]
  (merge-with + pos other_pos))

(def offsets
  {:right {:x 1 :y 0}
   :left {:x -1 :y 0}
   :up {:x 0 :y -1}
   :down {:x 0 :y 1}})

(defn- at_position
  [trail_map {x :x y :y}]
  (aget trail_map y x))

(def slope_directions
  {
   "<" :left
   ">" :right
   "v" :down
   "^" :up
   })

(defn- trail_end
  [trail_map trailhead direction start destination]
  (loop [trail_length 2 position (add_positions trailhead (get offsets direction)) previous trailhead]
    (if (contains? #{start destination} position)
      {:end position :length trail_length}
      (let [type (at_position trail_map position)
            next_position (->> (vals offsets)
                               (map #(add_positions position %1))
                               (filter #(not= previous %1))
                               (filter #(not= "#" (at_position trail_map %1)))
                               (first))]
        (if (contains? slope_directions type)
          {:end next_position :length (inc trail_length)}
          (recur (inc trail_length) next_position position))))))

(defn- outward-slopes
  [trail_map position]
  (->> (vals slope_directions)
       (filter #(= %1 (->> %1
                          (get offsets)
                          (add_positions position)
                          (at_position trail_map)
                          (get slope_directions))))
       (into [])))

(defn- trail_graph
  [trail_map start destination]
  (let [first_trail_result (trail_end trail_map start :down start destination)
        first_trail (update first_trail_result :length dec) ; The start itself does not contribute to the length.
        ]
    (loop [trails {start #{first_trail}} to_process #{(:end first_trail)}]
      (if (empty? to_process)
        trails
        (let [position (first to_process)]
          (if (= position destination)
            (recur trails (disj to_process position))
            (let [fresh_trails (->> (outward-slopes trail_map position)
                                (map #(trail_end
                                      trail_map
                                      (add_positions position (get offsets %1))
                                      %1
                                      start
                                      destination)))
                  new_trails (reduce #(update %1 position (fn [ends] (if (nil? ends) #{%2} (conj ends %2)))) trails fresh_trails)
                  fresh_nodes (->> fresh_trails
                                   (map :end)
                                   (filter #(not (contains? trails %1))))
                  new_to_process (disj (into to_process fresh_nodes) position)]
              (recur new_trails new_to_process))))))))


(defn- graph_size
  [graph]
  {:vertices (inc (count (keys graph))) :edges (reduce + (map count (vals graph)))})

(defn- topological_sort
  [graph]
  (let [initial_in_counts_base (reduce #(assoc %1 %2 0) {} (keys graph))
        initial_in_counts (reduce #(update %1 (:end %2) (fn [in_count] (if (nil? in_count) 1 (inc in_count))))
                                  initial_in_counts_base
                                  (reduce into [] (vals graph)))
        relevant_nodes (into #{} (keys initial_in_counts))]
    (loop [remaining_nodes relevant_nodes order [] in_counts initial_in_counts]
      (if (empty? remaining_nodes)
        order
        (let [starts (->> remaining_nodes
                          (filter #(= 0 (get in_counts %1))))]
          (if (empty? starts)
            nil                                             ; There is no topological order, i.e. this is not a DAG.
            (let [new_order (into order starts)
                  new_remaining_nodes (reduce disj remaining_nodes starts)
                  edge_ends (->> starts
                                 (map #(get graph %1 []))
                                 (map #(into [] %1))
                                 (reduce into [])
                                 (map :end))
                  new_in_counts (reduce #(update %1 %2 dec) in_counts edge_ends)]
              (recur new_remaining_nodes new_order new_in_counts))))))))


; In the following, we use that the trail graph is a DAG. In particular, there is a topological order.

(defn- update_longest_for_node
  [longest_paths node trails]
  (let [till_here (get longest_paths node nil)]
    (if (nil? till_here)
      longest_paths
      (let [out_edges (get trails node [])]
        (reduce #(update %1 (:end %2) (fn [old_longest]
                                        (let [new_path_longest (+ till_here (:length %2))]
                                          (if (nil? old_longest)
                                            new_path_longest
                                           (max old_longest new_path_longest)))))
                longest_paths
                out_edges)))))

(defn- longest-path
  [trails start destination]
  (let [topological_order (topological_sort trails)
        longest_paths (reduce #(update_longest_for_node %1 %2 trails) {start 0} topological_order)]
    (get longest_paths destination)))

(defn solve_part1
  "Solves part 1 of day 23 of AdventOfCode 2023"
  [input]
  (let [trail_map (to-array-2d (:trail_map input))
        trails (trail_graph trail_map (:start input) (:destination input))
        start (:start input)
        destination (:destination input)]
    ;(println (graph_size trails))
    ;(println (topological_sort trails))
    (str (longest-path trails start destination))))


(defn- non-slippery-outward-slopes
  [trail_map position]
  (->> (vals slope_directions)
       (filter #(->> %1
                     (get offsets)
                     (add_positions position)
                     (at_position trail_map)
                     (contains? slope_directions)))
       (into [])))

(defn- non_slippery_trail_graph
  [trail_map start destination]
  (let [first_trail_result (trail_end trail_map start :down start destination)
        first_trail (update first_trail_result :length dec) ; The start itself does not contribute to the length.
        ]
    (loop [trails {start #{first_trail}} to_process #{(:end first_trail)}]
      (if (empty? to_process)
        trails
        (let [position (first to_process)]
          (if (= position destination)
            (recur trails (disj to_process position))
            (let [fresh_trails (->> (non-slippery-outward-slopes trail_map position)
                                    (map #(trail_end
                                            trail_map
                                            (add_positions position (get offsets %1))
                                            %1
                                            start
                                            destination)))
                  new_trails (reduce #(update %1 position (fn [ends] (if (nil? ends) #{%2} (conj ends %2)))) trails fresh_trails)
                  fresh_nodes (->> fresh_trails
                                   (map :end)
                                   (filter #(not (contains? trails %1))))
                  new_to_process (disj (into to_process fresh_nodes) position)]
              (recur new_trails new_to_process))))))))

(defn- non-loop-path-graph
  [trail_graph start destination]
  (loop [graph {} to_process #{{:position start :visited #{start}}}]
    (if (empty? to_process)
      graph
      (let [current (first to_process)
            {position :position visited :visited} current]
        (if (= position destination)
          (recur graph (disj to_process current))
          (let [
                fresh_edges (->> (get trail_graph position)
                                 (filter #(not (contains? visited (:end %1))))
                                 (map #(update %1 :end (fn [end_point] {:position end_point :visited (conj visited end_point)}))))
                ;b (println current)
                ;a (println (str/join "\n" fresh_edges))
                new_graph (reduce #(update %1 current (fn [ends] (if (nil? ends) #{%2} (conj ends %2)))) graph fresh_edges)
                fresh_nodes (->> fresh_edges
                                 (map :end)
                                 (filter #(not (contains? graph %1))))
                new_to_process (disj (into to_process fresh_nodes) current)]
            (recur new_graph new_to_process)))))))

; The result of non-loop-path-graph is always a DAG by construction.

(defn- non-slippery-longest-path
  [non-slippery-trails start destination]
  (let [path_graph (time (non-loop-path-graph non-slippery-trails start destination))
        topological_order (topological_sort path_graph)
        longest_paths (reduce #(update_longest_for_node %1 %2 path_graph) {{:position start :visited #{start}} 0} topological_order)]
    (->> longest_paths
         (filter (fn [[{position :position} _]] (= position destination)))
         (map last)
         (apply max))))

(defn- edge_list_string
  [trails]
  (->> trails
       (map (fn [[start ends]] (into [] (map (fn [{end :end length :length}] [start end length]) ends))))
       (reduce into [])
       (map #(str/join "\t" %1))
       (str/join "\n")))

(defn- node_list_string
  [trails]
  (let [non_end_nodes (keys trails)
        end_nodes (->> trails
                         (vals)
                         (map :end)
                         (reduce into #{})
                         (filter #(not (contains? trails %1))))]
    (->> (concat non_end_nodes end_nodes)
         (str/join "\n"))))

(defn- non-slippery-longest-path-dfs-internal
  [non-slippery-trails destination point visited current-length]
  (if (= point destination)
    current-length
    (let [relevant-edges (->> (get non-slippery-trails point)
                            (filter (fn [{end :end}] (not (contains? visited end)))))]
      (if (empty? relevant-edges)
        -1                                                  ;path never reached the destination
        (let [longest-to-destination (map #(non-slippery-longest-path-dfs-internal
                                             non-slippery-trails
                                             destination
                                             (:end %1)
                                             (conj visited (:end %1))
                                             (+ current-length (:length %1)))
                                          relevant-edges)]
          (apply max longest-to-destination))
        ))))

(defn- non-slippery-longest-path-dfs
  [non-slippery-trails start destination]
  (non-slippery-longest-path-dfs-internal non-slippery-trails destination start #{start} 0))

(defn solve_part2
  "Solves part 2 of day 23 of AdventOfCode 2023"
  [input]
  (let [trail_map (to-array-2d (:trail_map input))
        trails (non_slippery_trail_graph trail_map (:start input) (:destination input))
        start (:start input)
        destination (:destination input)]
    (println (graph_size trails))
    ;(println (str/join "\n" (sort-by (fn [[{x :x y :y} _]] [x y]) trails)))
    ;(println (node_list_string trails))
    ;(println (edge_list_string trails))
    (str (non-slippery-longest-path-dfs trails start destination))))
