(ns net.slothrop.aoc2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-input [file]
  (with-open [rdr (io/reader (io/resource file))]
    (->> rdr
         line-seq
         (map #(.trim %))
         (filter #(not (zero? (.length %))))
         (into []))))

(def example (read-input "day18-example.txt"))
(def input (read-input "day18.txt"))

(defn mval [m x] (if (number? x) x (if-let [v (m x)] v 0)))
(defn msnd [m x] (assoc m :last-sound (mval m x)))
(defn mset [m x y] (assoc m x (mval m y)))
(defn madd [m x y] (update m x (fnil + 0) (mval m y)))
(defn mmul [m x y] (update m x (fnil * 0) (mval m y)))
(defn mmod [m x y] (update m x (fnil rem 0) (mval m y)))
(defn mrcv [m x] (if (zero? (mval m x)) m (assoc m :recovered (m :last-sound))))
(defn mjgz [m x y] (if (> (mval m x) 0) (update m :pc + (dec (mval m y))) m)) ;; dec because we increment pc for all instructions

(defn process-arg [arg]
  (try (Integer/parseInt arg)
       (catch NumberFormatException nfe (keyword arg))))

(defn step [program machine]
  (let [instruction (program (machine :pc))
        [command & rawargs] (s/split instruction #"\s+")
        args (map process-arg rawargs)]
    (update (apply (resolve (symbol (str "m" command))) machine args) :pc inc)))

(comment
  (first (drop-while #(nil? (% :recovered)) (iterate (partial step input) {:pc 0}))))

;; part2

(defn m2val [m x] (if (number? x) x (if-let [v (m x)] v 0)))
(defn m2snd [m o x] [(update m :send-count (fnil inc 0)) (update o :q conj (mval m x))])
(defn m2set [m o x y] [(assoc m x (mval m y)) o])
(defn m2add [m o x y] [(update m x (fnil + 0) (mval m y)) o])
(defn m2mul [m o x y] [(update m x (fnil * 0) (mval m y)) o])
(defn m2mod [m o x y] [(update m x (fnil rem 0) (mval m y)) o])
(defn m2rcv [m o x] [(if-let [val (peek (:q m))]
                       (-> m (assoc x val) (update :q pop) (assoc :waiting false))
                       (-> m (update :pc dec) (assoc :waiting true))) o])
(defn m2jgz [m o x y] [(if (> (mval m x) 0) (update m :pc + (dec (mval m y))) m) o]) ;; dec because we increment pc for all instructions

(defn step2 [program machine other-machine]
  (if (>= (machine :pc) (count program))
    [(update machine :terminated true) other-machine]
    (let [instruction (program (machine :pc))
          [command & rawargs] (s/split instruction #"\s+")
          args (map process-arg rawargs)]
      (update-in (apply (resolve (symbol (str "m2" command))) machine other-machine args) [0 :pc] inc))))

(defn two-step [program [machine0 machine1]]
  (let [[m1a m2a] (step2 program machine0 machine1)]
    (step2 program m2a m1a)))

(def example2 (read-input "day18-example2.txt"))

(comment
  (first (drop-while (fn [[m1 m2]] (and (or (not (:waiting m1))
                                            (not (:waiting m2)))
                                        (or (not (:terminated m1))
                                            (not (:terminated m2))))) (iterate (partial two-step input) [{:pc 0 :p 0 :q (clojure.lang.PersistentQueue/EMPTY)}
                                                                                                         {:pc 0 :p 1 :q (clojure.lang.PersistentQueue/EMPTY)}]))) 3


  )






