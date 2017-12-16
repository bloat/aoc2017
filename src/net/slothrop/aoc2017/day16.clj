(ns net.slothrop.aoc2017.day16
  (:require [clojure.java.io :as io]))

(defn spin [p x]
  (let [[t d] (split-at (- (count p) x) p)]
    (into [] (concat d t))))

(defn exchange [p a b]
  (-> p
      (assoc a (p b))
      (assoc b (p a))))

(defn partner [p a b]
  (letfn [(find-thing [n] (first (keep-indexed #(when (= %2 n) %1) p)))]
    (exchange p (find-thing a) (find-thing b))))

(def input
  (into [] (.split (slurp (io/resource "day16.txt")) ",")))

(defn step [p s]
  (cond (.startsWith s "s") (spin p (Integer/parseInt (.trim (.substring s 1))))
        (.startsWith s "x") (let [sl (.indexOf s (int \/))]
                              (exchange p
                                        (Integer/parseInt (.trim (.substring s 1 sl)))
                                        (Integer/parseInt (.trim (.substring s (inc sl))))))
        (.startsWith s "p") (let [sl (.indexOf s (int \/))]
                              (partner p
                                       (symbol (.trim (.substring s 1 sl)))
                                       (symbol (.trim (.substring s (inc sl))))))))

(defn dance [p i]
  (reduce step p i))

(dance '[a b c d e f g h i j k l m n o p] input)

;; Part 2
(second (keep-indexed (fn [i item] (when (= item '[a b c d e f g h i j k l m n o p])
                                     i))
                      (iterate (fn [t] (dance t input)) '[a b c d e f g h i j k l m n o p])))

(rem 1000000000 42)

(apply str (first (drop 34 (iterate (fn [t] (dance t input)) '[a b c d e f g h i j k l m n o p]))))
