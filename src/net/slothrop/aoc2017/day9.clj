(ns net.slothrop.aoc2017.day9
  (:require [clojure.java.io :as io]))

; Drop trailing newline
(def stream (let [s (slurp (io/resource "day9.txt"))]
              (.substring s 0 (dec (count s)))))

(defn group-tracker [[stack sum state :as all-state] c]
  (cond (and (= \{ c) (= state :in-group)) [(conj stack (inc (peek stack)))
                                            (+ sum (inc (peek stack)))
                                            :in-group]
        (and (= \} c) (= state :in-group)) [(pop stack) sum :in-group]
        (and (= \< c) (= state :in-group)) [stack sum :in-garbage]
        (and (= \> c) (= state :in-garbage)) [stack sum :in-group]
        (and (= \! c) (= state :in-garbage)) [stack sum :cancel]
        (= state :cancel) [stack sum :in-garbage]
        :else all-state))

(defn score [stream]
  (second (reduce group-tracker '[(0) 0 :in-group] stream)))

;; Part 2

(defn garbage-tracker [[garbage state :as all-state] c]
  (cond (and (= \< c) (= state :in-group)) [garbage :in-garbage]
        (and (= \> c) (= state :in-garbage)) [garbage :in-group]
        (and (= \! c) (= state :in-garbage)) [garbage :cancel]
        (= state :cancel) [garbage :in-garbage]
        (= state :in-garbage) [(inc garbage) :in-garbage]
        :else all-state))

(defn garbage [stream]
  (first (reduce garbage-tracker '[0 :in-group] stream)))

(comment

  (score    "{}")
  (score "{{{}}}")
  (score  "{{},{}}")
  (score  "{{{},{},{{}}}}")
  (score "{<a>,<a>,<a>,<a>}")
  (score  "{{<ab>},{<ab>},{<ab>},{<ab>}}")
  (score  "{{<!!>},{<!!>},{<!!>},{<!!>}}")
  (score  "{{<a!>},{<a!>},{<a!>},{<ab>}}")

  (score stream)

  (garbage    "{}")
  (garbage "{{{}}}")
  (garbage  "{{},{}}")
  (garbage  "{{{},{},{{}}}}")
  (garbage "{<a>,<a>,<a>,<a>}")
  (garbage  "{{<ab>},{<ab>},{<ab>},{<ab>}}")
  (garbage  "{{<!!>},{<!!>},{<!!>},{<!!>}}")
  (garbage  "{{<a!>},{<a!>},{<a!>},{<ab>}}")

  (garbage stream)

)

