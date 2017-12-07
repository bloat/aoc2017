(ns net.slothrop.aoc2017.day6
  (:require [clojure.java.io :as io]))

(set! *warn-on-reflection* true)

(def memory [0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11])

(defn largest [^ints memory]
  (let [memory-size (int (alength memory))]
    (loop [i (int 0)
           big (int -1)
           big-idx (int -1)]
      (if (= i memory-size)
        big-idx
        (if (> (aget memory i) big)
          (recur (inc i) (aget memory i) i)
          (recur (inc i) big big-idx))))))

(defn next-idx [idx memory-size]
  (let [new-idx (inc idx)]
    (if (= memory-size new-idx)
      0
      new-idx)))

(defn redist [^ints memory idx]
  (let [memory-size (int (alength memory))
        blocks (aget memory idx)]
    (aset memory idx 0)
    (loop [i (next-idx idx memory-size) b blocks]
      (when (not (zero? b))
        (aset memory i (inc (aget memory i)))
        (recur (next-idx i memory-size) (dec b))))))

(defn cycle [^ints memory]
  (loop [seen (hash-set (into [] memory)) count 1]
    (redist memory (largest memory))
    (let [new-config (into [] memory)]
      (if (contains? seen new-config)
        count
        (recur (conj seen new-config) (inc count))))))

(comment
  (cycle (int-array [10 9 8 7 6 5 4 3 1 1 0 15 14 13 11 12]))
  )
