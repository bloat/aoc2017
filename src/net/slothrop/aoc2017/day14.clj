(ns net.slothrop.aoc2017.day14
  (:require [net.slothrop.aoc2017.day10 :as knot]))

(defn knot [str-input]
  (knot/tohex (knot/dense (knot/round64 (knot/lengths str-input)))))

(defn bits [str]
  (reduce + (map {\0 0 \1 1 \2 1 \3 2 \4 1 \5 2 \6 2 \7 3
                  \8 1 \9 2 \a 2 \b 3 \c 2 \d 3 \e 3 \f 4} str)))

(defn rows [key]
  (map #(str key "-" (Integer/toString %)) (range 128)))

;; part 2

(defn digit->bool [d]
  (let [bits (map #(= \1 %) (Integer/toBinaryString (Character/digit d 16)))]
    (concat (take (- 4 (count bits)) [false false false]) bits)))

(defn bitrow [rowkey]
  (->> rowkey
       knot
       (mapcat digit->bool)
       (into [])))

(defn grid [key]
  (->> key
       rows
       (map bitrow)
       (into [])))

(defn start-nodes [grid]
  (for [x (range 128) y (range 128) :when ((grid x) y)] [x y]))

(defn grid-neighbours [[x y] grid]
  (for [dx [-1 0 1] dy [-1 0 1]
        :when (or (zero? dx) (zero? dy))
        :when (not (and (zero? dx) (zero? dy)))
        :let [newx (+ x dx)
              newy (+ y dy)]
        :when (and (< -1 newx 128)
                   (< -1 newy 128))
        :when ((grid newx) newy)]
    [newx newy]))

(defn traverse [start neighbours]
  (loop [v #{} s (list start)]
    (if (empty? s)
      v
      (let [node (first s)
            newv (conj v node)]
        (recur newv (into (rest s) (filter (comp not newv) (neighbours node))))))))

(defn groups [grid neighbours]
  (loop [nodes (start-nodes grid) g 0]
    (if (empty? nodes)
      g
      (recur (doall (filter (comp not (traverse (first nodes) neighbours)) nodes)) (inc g)))))


(comment
  (->> "jzgqcdpd"
       rows
       (map knot)
       (map bits)
       (reduce +))

  (def exgrid (grid "flqrgnkx"))
  (def mygrid (grid "jzgqcdpd"))

  (groups mygrid #(grid-neighbours % mygrid))
  )
