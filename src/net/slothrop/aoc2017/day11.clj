(ns net.slothrop.aoc2017.day11
  (:require [clojure.java.io :as io]))

(def moves (map #(keyword (.trim %)) (.split (slurp (io/resource "day11.txt")) ",")))

(def directions-oddx
  {:n [0 -1]
   :ne [1 0]
   :se [1 1]
   :s [0 1]
   :sw [-1 1]
   :nw [-1 0]})

(def directions-evenx
  {:n [0 -1]
   :ne [1 -1]
   :se [1 0]
   :s [0 1]
   :sw [-1 0]
   :nw [-1 -1]})

(defn move [[x y] m] (let [[dx dy] ((if (even? x) directions-evenx directions-oddx) m)]
                       (vector (+ x dx) (+ y dy))))

(def final (reduce move
                   [0 0]
                   moves))

(loop [p [0 0] c 0]
  (if (= (first p) (first final))
    (+ c (- (second final) (second p)))
    (recur (move p :sw) (inc c))))

;; part 2

(def all-positions (reductions move [0 0] moves))

(defn dist [p]
  (let [x (first p)
        y (second p)
        dir (cond (and (pos? x) (pos? y)) :se
                  (and (pos? x) (neg? y)) :ne
                  (and (neg? x) (pos? y)) :sw
                  (and (neg? x) (neg? y)) :nw
                  :else :ne)]
    (loop [np [0 0] c 0]
      (cond (= (first np) (first p)) (+ c (Math/abs (- (second p) (second np))))
            (= (second np) (second p)) (+ c (Math/abs (- (first p) (first np))))
            :else (recur (move np dir) (inc c))))))

(apply max (map dist all-positions))


