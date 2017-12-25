(ns net.slothrop.aoc2017.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(defn step [[tape head state]]
  (condp = state
    :a (if (tape head)
         [(disj tape head)
          (dec head)
          :f]
         [(conj tape head)
          (inc head)
          :b])
    :b (if (tape head)
         [(disj tape head)
          (inc head)
          :d]
         [tape
          (inc head)
          :c])
    :c (if (tape head)
         [tape
          (inc head)
          :e]
         [(conj tape head)
          (dec head)
          :d])
    :d (if (tape head)
         [(disj tape head)
          (dec head)
          :d]
         [tape
          (dec head)
          :e])
    :e (if (tape head)
         [tape
          (inc head)
          :c]
         [tape
          (inc head)
          :a])
    :f (if (tape head)
         [tape
          (inc head)
          :a]
         [(conj tape head)
          (dec head)
          :a])))

(count (first (nth (iterate step [#{} 0 :a]) 12994925)))
