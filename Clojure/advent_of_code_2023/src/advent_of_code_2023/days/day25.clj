(ns advent-of-code-2023.days.day25
  (:require [clojure.string :as str])
  (:require [clojure.data.priority-map :refer [priority-map]]))


(defn- parse-node
  [text]
  (let [parts (re-find #"([^:]+): (.+)" text)
        node (get parts 1)
        edges (into #{} (-> (get parts 2)
                        (str/trim)
                        (str/split #" ")))]
    [node edges]))

(defn- parse-edge-map
  [text]
  (->> (str/split text #"\r?\n")
       (remove str/blank?)
       (map parse-node)
       (into {})))

(defn parse_input
  "Parses the input for day 25 of AdventOfCode 2023"
  [input_string]
  (parse-edge-map input_string))


(defn- completed-graph
  [graph]
  (reduce
    (fn [old_graph [node end]] (update old_graph node #(if (nil? %1) #{end} (conj %1 end))))
    graph
    (->> graph
         (map (fn [[node ends]] (map (fn [end] [end node]) ends)))
         (reduce into []))))

(defn- partition_heuristically
  [graph edges_to_cut]
  (let [initial_group #{}
        initial-score-fn (fn [node ends] (* (if (contains? initial_group node) -1 1) (reduce + (map #(if (contains? initial_group %1) -1 1) ends))))
        outwards-count-fn (fn [[_ ends]] (reduce + (map #(if (contains? initial_group %1) 0 1) ends)))
        initial_scored_nodes (->> graph
                                  (map (fn [[node edges]] [node (initial-score-fn node edges)]))
                                  (reduce into [])
                                  (apply priority-map))
        initial_outwards_count (->> graph
                                    (filter (fn [[node _]] (contains? initial_group node)))
                                    (map outwards-count-fn)
                                    (reduce +))]
    (println initial_outwards_count)
    (println initial_scored_nodes)
    (loop [scored_nodes initial_scored_nodes group initial_group outward_count initial_outwards_count]
      (let [[next_member score] (peek scored_nodes)]
        (if (contains? group next_member)
          (let [new_group (disj group next_member)
                new_outward_count (+ outward_count score)]
            (if (<= new_outward_count edges_to_cut)
              new_group
              (let [new_scored_nodes (reduce
                                       #(if (contains? %1 %2) (update %1 %2 (fn [score] (+ score (if (contains? group %2) -2 2)))) %1)
                                       (pop scored_nodes)
                                       (get graph next_member))]
                (if (empty? new_scored_nodes)
                  nil
                  (recur new_scored_nodes new_group new_outward_count)))))
          (let [new_group (conj group next_member)
                new_outward_count (+ outward_count score)]
            (if (<= new_outward_count edges_to_cut)
              new_group
              (let [new_scored_nodes (reduce
                                       #(if (contains? %1 %2) (update %1 %2 (fn [score] (+ score (if (contains? group %2) 2 -2)))) %1)
                                       (pop scored_nodes)
                                       (get graph next_member))]
                (if (empty? new_scored_nodes)
                  nil
                  (recur new_scored_nodes new_group new_outward_count))))))))))

(defn solve_part1
  "Solves part 1 of day 25 of AdventOfCode 2023"
  [input]
  (let [graph (completed-graph input)
        first_group (partition_heuristically graph 3)
        full_count (count graph)
        first_count (count first_group)]
    (str (* first_count (- full_count first_count)))))

