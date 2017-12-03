(ns net.slothrop.aoc2017.day3)

(def sides {:bottom :right
            :right :top
            :top :left
            :left :bottom})

(defn spiral [n]

  (loop [address 1
         x 0 y 0
         side :bottom
         countdown 0
         new-countdown 0]

    (if (= address n)
      (+ (Math/abs x) (Math/abs y))
      (recur (inc address)

             (cond (= :bottom side) (inc x)
                   (= :right side) x
                   (= :top side) (dec x)
                   (= :left side) x)

             (cond (= :bottom side) y
                   (= :right side) (inc y)
                   (= :top side) y
                   (= :left side) (dec y))

             (if (zero? countdown)
               (sides side)
               side)

             (if (zero? countdown)
               new-countdown
               (dec countdown))

             (if (zero? countdown)
               (cond (= :bottom side) (inc new-countdown)
                     (= :right side) new-countdown
                     (= :top side) (inc new-countdown)
                     (= :left side) new-countdown)
               new-countdown)))))

(comment

  (spiral 361527))

;; Part 2

(def deltas [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]])

(defn adj-sum [x y grid]
  (->> deltas
       (map (fn [[dx dy]] (vector (+ x dx) (+ y dy))))
       (map #(get grid % 0))
       (reduce +)))

(defn fill-spiral [n]

  (loop [address 1
         x 0 y 0
         side :bottom
         countdown 0
         new-countdown 0
         grid {[0 0] 1}]

    (if (> (grid (vector x y)) n)

      (grid (vector x y))

      (let [newx (cond (= :bottom side) (inc x)
                       (= :right side) x
                       (= :top side) (dec x)
                       (= :left side) x)

            newy (cond (= :bottom side) y
                       (= :right side) (inc y)
                       (= :top side) y
                       (= :left side) (dec y))]
        
        (recur (inc address)

               newx newy

               (if (zero? countdown)
                 (sides side)
                 side)

               (if (zero? countdown)
                 new-countdown
                 (dec countdown))

               (if (zero? countdown)
                 (cond (= :bottom side) (inc new-countdown)
                       (= :right side) new-countdown
                       (= :top side) (inc new-countdown)
                       (= :left side) new-countdown)
                 new-countdown)

               (assoc grid (vector newx newy) (adj-sum newx newy grid)))))))

(comment

  (fill-spiral 361527)

  )





