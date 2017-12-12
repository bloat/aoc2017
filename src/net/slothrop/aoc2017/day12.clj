(ns net.slothrop.aoc2017.day12
  (:require [clojure.java.io :as io]))

(defn graph-entry [line]
  (let [split1 (.split line "<->")
        split2 (.split (aget split1 1) ",")]
    (vector (Integer/parseInt (.trim (aget split1 0)))
            (into [] (map #(Integer/parseInt (.trim %)) split2)))))

(def graph (with-open [rdr (io/reader  (io/resource "day12.txt"))]
             (reduce (fn [g [k v]]
                       (update-in g [k] (fn [a] (if (nil? a) v (into a v))))) {} (map graph-entry (line-seq rdr)))))

(defn traverse [start graph]
  (loop [v #{} s (list start)]
    (if (empty? s)
      v
      (let [node (first s)
            newv (conj v node)]
        (recur newv (into (rest s) (filter (comp not newv) (graph node))))))))

;; Part 2

(defn groups [graph]
  (loop [nodes (keys graph) g 0]
    (if (empty? nodes)
      g
      (recur (filter (comp not (traverse (first nodes) graph)) nodes) (inc g)))))

(comment

  (count (traverse 0 graph))
  (groups graph)
  )
