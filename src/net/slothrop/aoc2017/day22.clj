(ns net.slothrop.aoc2017.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn load-grid [file]
  (with-open [rdr (io/reader (io/resource file))]
    (into #{}
          (map first (filter (fn [[_ c]] (= \# c))
                             (for [row (map vector (line-seq rdr) (range))
                                   :let [[row-data row-num] row]
                                   cell (map vector row-data (range))] [[row-num (second cell)] (first cell)]))))))

(def the-grid (load-grid "day22.txt"))
(def ex-grid (load-grid "day22-ex.txt"))

(def deltas [[-1 0] [0 1] [1 0] [0 -1]])

(defn move [[grid pos delta icount]]
  (let [new-delta (mod (if (grid pos)
                         (inc delta)
                         (dec delta)) 4)]
    [(if (not (grid pos))
       (conj grid pos)
       (disj grid pos))
     [(+ (first pos) (first (deltas new-delta)))
      (+ (second pos) (second (deltas new-delta)))]
     new-delta
     (if (not (grid pos))
       (inc icount)
       icount)]))

;; part2


(def ex-grid-2 (into {} (map vector ex-grid (repeat :infected))))
(def the-grid-2 (into {} (map vector the-grid (repeat :infected))))

(def states {:clean :weak :weak :infected :infected :flagged :flagged :clean})

(defn move2 [[grid pos delta icount]]
  (when (= (rem icount 1000) 0)
    (println icount))
  (let [new-delta (mod (let [state (grid pos)]
                         (cond (nil? state) (dec delta)
                               (= :clean state) (dec delta)
                               (= :infected state) (inc delta)
                               (= :flagged state) (+ 2 delta)
                               :else delta)) 4)]
    [(if (not (grid pos))
       (assoc grid pos :weak)
       (update grid pos states))
     [(+ (first pos) (first (deltas new-delta)))
      (+ (second pos) (second (deltas new-delta)))]
     new-delta
     (if (= :weak (grid pos))
       (inc icount)
       icount)]))

(comment
  (last (nth (iterate move [the-grid [12 12] 0 0]) 10000))

  (last (nth (iterate move2 [the-grid-2 [12 12] 0 0]) 10000000))
  )
