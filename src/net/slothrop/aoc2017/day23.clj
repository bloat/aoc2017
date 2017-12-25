(ns net.slothrop.aoc2017.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-input [file]
  (with-open [rdr (io/reader (io/resource file))]
    (->> rdr
         line-seq
         (map #(.trim %))
         (filter #(not (zero? (.length %))))
         (into []))))

(def input (read-input "day23.txt"))

(defn mval [m x] (if (number? x) x (if-let [v (m x)] v 0)))
(defn mset [m x y] (assoc m x (mval m y)))
(defn msub [m x y] (when (= x :h) (println m)) (update m x (fnil - 0) (mval m y)))
(defn mmul [m x y] (-> m (update x (fnil * 0) (mval m y)) (update :mul-count inc)))
(defn mjnz [m x y] (if (not= (mval m x) 0) (update m :pc + (dec (mval m y))) m)) ;; dec because we increment pc for all instructions

(defn process-arg [arg]
  (try (Integer/parseInt arg)
       (catch NumberFormatException nfe (keyword arg))))

(defn step [program machine]
  (if (not (< -1 (machine :pc) (count program)))
    (:mul-count machine)
    (let [instruction (program (machine :pc))
          [command & rawargs] (s/split instruction #"\s+")
          args (map process-arg rawargs)]
      (println (str machine))
      (println instruction)
      (update (apply (resolve (symbol (str "m" command))) machine args) :pc inc))))

(comment
  (with-open [wri (io/writer "log.txt")]
    (binding [*out* wri]
      (doall (nth (iterate (partial step input) {:pc 0 :mul-count 0 :a 1}) 5000000))))
  )
