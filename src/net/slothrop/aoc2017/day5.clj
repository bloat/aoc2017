(ns net.slothrop.aoc2017.day5
  (:require [clojure.java.io :as io]))

(def maze
  (with-open [r (java.io.PushbackReader. (io/reader (io/resource "day5.txt")))]
    (read r)))

(defn jump [initial-maze update-fn]
  (loop [pc 0 maze initial-maze steps 0]

    (if (or (< pc 0) (>= pc (count maze)))
      steps

      (recur (+ pc (maze pc))
             (update-in maze [pc] update-fn)
             (inc steps)))))

(jump maze inc)

;; Part 2

(jump maze
      (fn [offset] (if (>= offset 3) (dec offset) (inc offset))))





