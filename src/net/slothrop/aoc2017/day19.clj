(ns net.slothrop.aoc2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn load-maze [file]
  (with-open [rdr (io/reader (io/resource file))]
    (into [] (map #(into [] %) (line-seq rdr)))))

(defn start-point [maze]
  [0 (first (keep-indexed (fn [i c] (when (= c \|) i)) (first maze)))])

(def dirs #{:up :down :left :right})

(def op
  {:down :up
   :up :down
   :left :right
   :right :left})

(defn move-up [maze [row col]]
  (let [new-row (dec row)]
    (if (not= \space (get (get maze new-row) col))
      [[new-row col] :up])))

(defn move-down [maze [row col]]
  (let [new-row (inc row)]
    (if (not= \space (get (get maze new-row) col))
      [[new-row col] :down])))

(defn move-left [maze [row col]]
  (let [new-col (dec col)]
    (if (not= \space (get (get maze row) new-col))
      [[row new-col] :left])))

(defn move-right [maze [row col]]
  (let [new-col (inc col)]
    (if (not= \space (get (get maze row) new-col))
      [[row new-col] :right])))

(def move-fns {:up move-up
               :down move-down
               :left move-left
               :right move-right})

(defn move [maze pos dir]
  (loop [letters [] [row col :as pos] pos dir dir]
    (let [new-letters
          (let [poschar (get (get maze row) col)]
            (if (Character/isAlphabetic (int poschar))
              (conj letters poschar)
              letters))]
      (if-let [[newpos _] ((move-fns dir) maze pos)]
        (recur new-letters newpos dir)
        (if-let [[newpos newdir] (some identity (map #((move-fns %) maze pos) (disj dirs dir (op dir))))]
          (recur new-letters newpos newdir)
          new-letters)))))

(def the-maze (load-maze "day19.txt"))
(def example-maze (load-maze "day19-example.txt"))

;; part 2

(defn move2 [maze pos dir]
  (loop [[row col :as pos] pos dir dir steps 1]
    (if-let [[newpos _] ((move-fns dir) maze pos)]
      (recur newpos dir (inc steps))
      (if-let [[newpos newdir] (some identity (map #((move-fns %) maze pos) (disj dirs dir (op dir))))]
        (recur newpos newdir (inc steps))
        steps))))

(comment
  (apply str (move the-maze (start-point the-maze) :down))"YOHREPXWN"
  (move2 the-maze (start-point the-maze) :down)
  
  )
