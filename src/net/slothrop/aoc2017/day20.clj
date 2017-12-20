(ns net.slothrop.aoc2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def particle-regex #"p=<([0-9-]+),([0-9-]+),([0-9-]+)>,\s+v=<([0-9-]+),([0-9-]+),([0-9-]+)>,\s+a=<([0-9-]+),([0-9-]+),([0-9-]+)>")

(defn parse-particle [line]
  (->> line
       (re-find particle-regex)
       rest
       (map #(Integer/parseInt %))
       (into [])))

(defn load-particles [file]
  (with-open [rdr (io/reader (io/resource file))]
    (into [] (map parse-particle (line-seq rdr)))))

(def particles (load-particles "day20.txt"))

(defn move [[px py pz vx vy vz ax ay az]]
  (let [nvx (+ vx ax)
        nvy (+ vy ay)
        nvz (+ vz az)
        npx (+ px nvx)
        npy (+ py nvy)
        npz (+ pz nvz)]
    [npx npy npz nvx nvy nvz ax ay az]))

(defn tick [particles]
  (map move particles))

(defn dist [& pos]
  (reduce + (map #(Math/abs %) pos)))

(defn mindist [particles]
  (->> particles
       (map (fn [[px py pz & _]] (dist px py pz)))
       (map vector (range))
       (reduce (fn [[i1 d1 :as p1] [i2 d2 :as p2]] (if (< d1 d2) p1 p2)))))

;; part2

(defn tick2 [particles]
  (->> particles
       (map move)
       (group-by #(take 3 %))
       (filter (fn [[_ particles]] (= 1 (count particles))))
       (map second)
       (map first)))

(comment

  (mindist (nth (iterate tick particles) 1500))
  (count (nth (iterate tick2 particles) 2000))
  
  )
