(ns net.slothrop.aoc2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn rot2 [[[r0c0 r0c1]
             [r1c0 r1c1]]]
  [[r1c0 r0c0]
   [r1c1 r0c1]])

(defn vflip2 [[[r0c0 r0c1]
               [r1c0 r1c1]]]
  [[r0c1 r0c0]
   [r1c1 r1c0]])

(defn hflip2 [[[r0c0 r0c1]
               [r1c0 r1c1]]]
  [[r1c0 r1c1]
   [r0c0 r0c1]])

(defn all2 [m]
  (let [rots (take 4 (iterate rot2 m))]
    (concat rots (map hflip2 rots) (map vflip2 rots) (map (comp hflip2 vflip2) rots))))

(defn rot3 [[[r0c0 r0c1 r0c2]
             [r1c0 r1c1 r1c2]
             [r2c0 r2c1 r2c2]]]
  [[r2c0 r1c0 r0c0]
   [r2c1 r1c1 r0c1]
   [r2c2 r1c2 r0c2]])

(defn vflip3 [[[r0c0 r0c1 r0c2]
               [r1c0 r1c1 r1c2]
               [r2c0 r2c1 r2c2]]]
   [[r0c2 r0c1 r0c0]
    [r1c2 r1c1 r1c0]
    [r2c2 r2c1 r2c0]])

(defn hflip3 [[[r0c0 r0c1 r0c2]
               [r1c0 r1c1 r1c2]
               [r2c0 r2c1 r2c2]]]
  [[r2c0 r2c1 r2c2]
   [r1c0 r1c1 r1c2]
   [r0c0 r0c1 r0c2]])

(defn all3 [m]
  (let [rots (take 4 (iterate rot3 m))]
    (concat rots (map hflip3 rots) (map vflip3 rots) (map (comp hflip3 vflip3) rots))))

(def rule2-regex #"([.#]{2})/([.#]{2}) => ([.#]{3})/([.#]{3})/([.#]{3})")
(def rule3-regex #"([.#]{3})/([.#]{3})/([.#]{3}) => ([.#]{4})/([.#]{4})/([.#]{4})/([.#]{4})")

(defn parse-line [line]
  (if-let [match (re-find rule2-regex line)]
    (let [im [(into [] (nth match 1))
              (into [] (nth match 2))]
          om [(into [] (nth match 3))
              (into [] (nth match 4))
              (into [] (nth match 5))]]
      (map #(vector % om) (all2 im)))
    (if-let [match (re-find rule3-regex line)]
      (let [im [(into [] (nth match 1))
                (into [] (nth match 2))
                (into [] (nth match 3))]
            om [(into [] (nth match 4))
                (into [] (nth match 5))
                (into [] (nth match 6))
                (into [] (nth match 7))]]
        (map #(vector % om) (all3 im))))))

(defn load-rules [file]
  (with-open [rdr (io/reader (io/resource file))]
    (into {} (mapcat parse-line (line-seq rdr)))))

(defn indexes [{data :data size :size} n]
  (for [r (range 0 size n)
        c (range 0 size n)]
    (for [ir (range r (+ r n))
          ic (range c (+ c n))]
      (+ (* size ir) ic))))

(defn key [data ilist n]
  (->> ilist
       (map data)
       (partition n)
       (map #(into [] %))
       (into [])))

(def ex-rules (load-rules "day21-example.txt"))
(def the-rules (load-rules "day21.txt"))

(defn new-size [s]
  (if (zero? (rem s 2))
    (int (* 3/2 s))
    (int (* 4/3 s))))

(defn flatten-squares [squares nss]
  (mapcat (fn [i] (mapcat (fn [square] (nth square i)) squares)) (range 0 nss)))

(defn flatten-m [new-square-size new-size squares]
  (into [] (mapcat #(flatten-squares % new-square-size) (partition (/ new-size new-square-size) squares))))

(defn enhance [m rules]
  (let [size (:size m)
        new-squares-are-threes (zero? (rem size 2))
        idx (if new-squares-are-threes (indexes m 2) (indexes m 3))
        new-size (new-size size)
        new-square-size (if new-squares-are-threes 3 4)]
    {:data (flatten-m new-square-size new-size (map rules (map #(key (:data m) % (if new-squares-are-threes 2 3)) idx)))
     :size new-size}))

(def start {:data (into [] ".#...####") :size 3})

(comment
  (map #(key (:data start) % 3) (divide start 3))
  (count (filter #(= \# %) (:data (nth (iterate #(enhance % the-rules) start) 18))))
  (enhance s2 ex-rules)
  (t (divide (first s2) 2))

  (filter nil?  (map the-rules  (all3 [[\. \# \.]
                                       [\. \. \#]
                                       [\# \# \#]])))
  )
