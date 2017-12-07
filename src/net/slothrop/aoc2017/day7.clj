(ns net.slothrop.aoc2017.day7
  (:require [clojure.java.io :as io]))

(def input
  (with-open [r (io/reader (io/resource "day7.txt"))]
    (into [] (line-seq r))))

(def example
  (with-open [r (io/reader (io/resource "day7-example.txt"))]
    (into [] (line-seq r))))

(defrecord Node [name weight children total-weight])

(defn add-node
  ([nodes name]
   (if (contains? nodes name)
     nodes
     (assoc nodes name (Node. name nil nil nil))))
  ([nodes name weight]
   (if (contains? nodes name)
     (assoc-in nodes [name :weight] weight)
     (assoc nodes name (Node. name weight nil nil)))))

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

;; Part 2

(defn total-node-weight [nodes node]
  (if (:total-weight (nodes node))
    nodes

    (if (empty? (:children (nodes node)))
      (assoc-in nodes [node :total-weight] (:weight (nodes node)))
      (let [nodes-with-populated-children
            (reduce total-node-weight
                    nodes
                    (:children (nodes node)))]
        (assoc-in nodes-with-populated-children [node :total-weight]
                  (reduce +
                          (:weight (nodes-with-populated-children node))
                          (map (fn [child] (:total-weight (nodes-with-populated-children child)))
                               (:children (nodes-with-populated-children node)))))))))

(defn bad-child [nodes node]
  (let [children (:children (nodes node))]
    (when (> (count children) 2)
      (->> children
           (map (fn [child] [child (:total-weight (nodes child))]))
           (group-by second)
           (filter (fn [[k v]] (= 1 (count v))))
           first
           second
           first
           first))))


(defn balanced? [nodes node]
  (= 1 (count (into #{} ))))

(comment
  (def exnodes (let [n (load-nodes example)]
                 (total-node-weight n (base-node n))))

  (def mynodes (let [n (load-nodes input)]
                 (total-node-weight n (base-node n))))

  (base-node mynodes)
  (take-while identity (iterate #(bad-child mynodes %) :svugo))
  (map (fn [c] (:total-weight (mynodes c))) (:children (mynodes :yruivis)))
  (mynodes :sphbbz)
  )
