(ns net.slothrop.aoc2017.day4
  (:require [clojure.java.io :as io]))

(def pass (map #(.split % "\\s+")
               (.split (slurp (io/resource "day4.txt")) "\n")))

(defn valid [a]
  (= (count a) (count (into #{} a))))

(comment
  (count (filter valid pass)))

;; part 2

(defn anag-valid [a]
  (= (count a) (count (into #{} (map sort a)))))

(comment
  (count (filter anag-valid pass)))

