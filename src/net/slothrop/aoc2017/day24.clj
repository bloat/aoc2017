(ns net.slothrop.aoc2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (with-open [rdr (io/reader (io/resource "day24.txt"))]
             (let [bridges (map
                            (fn [s] (let [slash (.indexOf s (int \/))
                                          a (Integer/parseInt (.substring s 0 slash))
                                          b (Integer/parseInt (.substring s (inc slash)))]
                                      [a b]))
                            (line-seq rdr))]
               (reduce (fn [m [a b :as c]]
                         (-> m
                             (update a (fnil conj #{}) c)
                             (update b (fnil conj #{}) c)))
                       {}
                       bridges))))

(defn strongest [bridges start]
  (let [br (bridges start)]
    (if (empty? br)
      [0 []]
      (apply max-key
             first
             (map (fn [[a b :as c]]
                      (let [[sum comps] (strongest
                                         (-> bridges
                                             (update start disj c)
                                             (update (if (= a start) b a) disj c))
                                         (if (= a start) b a))]
                        [(+ sum a b) (conj comps c)]))
                    br)))))

;; part 2

(defn longest [bridges start]
  (let [br (bridges start)]
    (if (empty? br)
      [0 0 []]
      (apply max-key
             (fn [[a b _]] (+ (* 10000 a) b))
             (map (fn [[a b :as c]]
                    (let [[length strength comps] (longest
                                                   (-> bridges
                                                       (update start disj c)
                                                       (update (if (= a start) b a) disj c))
                                                   (if (= a start) b a))]
                      [(inc length) (+ strength a b) (conj comps c)]))
                  br)))))

(comment
  (strongest input 0)
  (longest input 0)
  )

