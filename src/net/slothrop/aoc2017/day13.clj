(ns net.slothrop.aoc2017.day13
  (:require [clojure.java.io :as io]))

(defn update-scanner [[d r p ud :as s]]
  (when s
    (cond (zero? p) (vector d r 1 :down)
          (= p (dec r)) (vector d r (- r 2) :up)
          (= :down ud) (vector d r (inc p) :down)
          (= :up ud) (vector d r (dec p) :up))))

(defn update-score [score position scanners]
  (if (< position (count scanners))
    (if-let [[d r p _] (scanners position)]
      (if (zero? p)
        (+ score (* d r))
        score)
      score)))

(defn picosecond [[scanners position score scorefn]]
;  (println (str (if (= -1 position) "I" position) " " scanners " " score))
  (let [new-position (inc position)]
    (vector
     (into [] (map update-scanner scanners))
     new-position
     (scorefn score new-position scanners)
     scorefn)))

(defn score [scanners]
  (nth (last (take-while (fn [[_ p _]] (< p (count scanners)))
                         (iterate picosecond [scanners -1 0 update-score])))
       2))

(def input
  {0 3
   1 2
   2 4
   4 4
   6 5
   8 6
   10 8
   12 8
   14 6
   16 6
   18 8
   20 8
   22 6
   24 12
   26 9
   28 12
   30 8
   32 14
   34 12
   36 8
   38 14
   40 12
   42 12
   44 12
   46 14
   48 12
   50 14
   52 12
   54 10
   56 14
   58 12
   60 14
   62 14
   66 10
   68 14
   74 14
   76 12
   78 14
   80 20
   86 18
   92 14
   94 20
   96 18
   98 17})

(def scanners
  (let [v (into [] (repeat (inc (apply max (keys input))) nil))]
    (reduce (fn [v [d r]] (assoc v d (vector d r 0 :down))) v input)))

;; Part 2

(defn pico-delay [scanners]
  (into [] (map update-scanner scanners)))

(defn delays [scanners]
  (map vector (range) (iterate pico-delay scanners)))

(defn caught-check [_ position scanners]
  (if (< position (count scanners))
    (if-let [[d r p _] (scanners position)]
      (zero? p)
      false)
    false))

(defn caught [scanners]
  (not= (count (take-while (fn [[_ p caught _ ]] (and (not caught) (< p (count scanners))))
                           (iterate picosecond [scanners -1 false caught-check])))
        (inc (count scanners))))

(defn caught-delays [scanners]
  (map (fn [[d i]] (if (= (mod d 10000) 0) (println d)) (vector d (caught i))) (delays scanners)))


(comment

  (def example [[0 3 0 :down]
                [1 2 0 :down]
                nil
                nil
                [4 4 0 :down]
                nil
                [6 4 0 :down]])
  (score example)
  (score scanners)

  (first (filter (comp not second) (caught-delays scanners)))
  
  )
