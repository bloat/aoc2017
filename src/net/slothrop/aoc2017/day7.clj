(ns net.slothrop.aoc2017.day7
  (:require [clojure.java.io :as io]))

(def input
  (with-open [r (io/reader (io/resource "day7.txt"))]
    (into [] (line-seq r))))

(def example
  (with-open [r (io/reader (io/resource "day7-example.txt"))]
    (into [] (line-seq r))))

(defrecord Node [name weight children])

(defn add-node
  ([nodes name]
   (if (contains? nodes name)
     nodes
     (assoc nodes name (Node. name nil nil))))
  ([nodes name weight]
   (if (contains? nodes name)
     (assoc-in nodes [name :weight] weight)
     (assoc nodes name (Node. name weight nil)))))

(defn get-name [^String line]
  (keyword (.substring line 0 (.indexOf line (int \space)))))

(defn get-weight [^String line]
  (Integer/parseInt (.substring line
                                (inc (.indexOf line (int \()))
                                (.indexOf line (int \))))))

(defn get-children [^String line]
  (if (.contains line "->")
    (into [] (map keyword (.split (.substring line (+ 2 (.indexOf line (int \>)))) ", ")))
    (vector)))

(defn add-children [nodes node-name children]
  (assoc-in (reduce add-node nodes children) [node-name :children] children))

(defn load-nodes [lines]
  (reduce (fn [nodes line]
            (let [node-name (get-name line)]
              (add-children
               (add-node
                nodes
                node-name
                (get-weight line))
               node-name
               (get-children line))))
          {}
          lines))

(defn base-node [nodes]
  (first (clojure.set/difference
          (into #{} (keys nodes))
          (into #{} (mapcat :children (vals nodes))))))

(comment
  (def exnodes (load-nodes example))
  (def mynodes (load-nodes input))
  (base-node mynodes)

  (map get-children example))
