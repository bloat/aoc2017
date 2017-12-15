(ns net.slothrop.aoc2017.day15)

(set! *warn-on-reflection* true)

(defn gen ^long [^long p ^long f]
  (rem (* p f) 2147483647))

(defn gena ^long [^long p]
  (gen p (long 16807)))

(defn genb ^long [^long p]
  (gen p (long 48271)))

(defn complow16 [^long a ^long b]
  (= (bit-and a 65535) (bit-and b 65535)))

(defn fortymil [^long a ^long b]
  (loop [a a b b i (int 40000000) c (int 0)]
    (if (zero? i)
      c
      (let [na (gena a)
            nb (genb b)]
        (if (complow16 na nb)
          (recur na nb (dec i) (inc c))
          (recur na nb (dec i) c))))))

;; Part 2

(defn gena2 ^long [^long p]
  (let [c (gen p (long 16807))]
    (if (zero? (bit-and c 3))
      c
      (recur c))))

(defn genb2 ^long [^long p]
  (let [c (gen p (long 48271))]
    (if (zero? (bit-and c 7))
      c
      (recur c))))

(defn fivemil [^long a ^long b]
  (loop [a a b b i (int 5000000) c (int 0)]
    (if (zero? i)
      c
      (let [na (gena2 a)
            nb (genb2 b)]
        (if (complow16 na nb)
          (recur na nb (dec i) (inc c))
          (recur na nb (dec i) c))))))

(comment
  (take 5 (drop 1 (iterate gena2 65)))
  (take 5 (drop 1 (iterate genb2 8921)))

  (fortymil 873 583)
  (fivemil 873 583)
  )



