(ns net.slothrop.aoc2017.day17)

(defn insert-spin [[step buffer i n]]
  (let [pos (rem (+ i step) (count buffer))
        [f b] (split-at (inc pos) buffer)]
    (vector step (concat f (vector (inc n)) b) (inc pos) (inc n))))

(defn after2017 [step]
  (last (take-while (fn [[_ _ _ n]] (< n 2018))
                    (iterate insert-spin [step [0] 0 0]))))

(defn nextcell [buffer]
  (second (drop-while #(not= % 2017) buffer)))

;; part2

(defn after-zero [^long step]
  (loop [bufsize (int 1) i (int 0) n (int 0) az (int 0)]
    (let [pos (rem (+ i step) bufsize)]
      (if (= 50000000 n)
        az
        (recur (+ 1 bufsize) (+ 1 pos) (+ 1 n) (if (zero? pos) (+ 1 n) az))))))


(comment
  (nextcell (second (after2017 3)))
  (nextcell (second (after2017 376)))

  (time (after-zero 376))

  )

