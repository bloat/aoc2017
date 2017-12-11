(ns net.slothrop.aoc2017.day10)

(def input [106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118])

(defn normal-reverse [l p n]
  (into [] (concat (take p l)
                   (clojure.core/reverse (subvec l p (+ p n)))
                   (drop (+ p n) l))))

(defn wrapped-reverse [l p n]
  (let [front-count (- n (- (count l) p))
        reversed (into [] (clojure.core/reverse (into (subvec l p) (subvec l 0 front-count))))
        reverse-split (- (count reversed) front-count)]
    (into [] (concat (subvec reversed reverse-split)
                     (subvec l front-count p)
                     (subvec reversed 0 reverse-split)))))

(defn is-wrapped? [l p n]
  (> (+ p n) (count l)))

(defn reverse-segment [l p n]
  (if (is-wrapped? l p n)
    (wrapped-reverse l p n)
    (normal-reverse l p n)))

(defn knot [[l p s] n]
  (vector (reverse-segment l p n) (rem (+ p s n) (count l)) (inc s)))

;; Part 2

(def str-input "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118")
(defn lengths [i] (into [] (concat (map int i) [17, 31, 73, 47, 23])))

(defn round [[l p s i]]
  (into [] (conj (reduce knot [l p s] i) i)))

(defn round64 [i]
  (first (nth (iterate round [(into [] (range 0 256)) 0 0 i]) 64)))

(defn dense [sparse]
  (map (fn [sixteen] (apply bit-xor sixteen)) (partition 16 sparse)))

(defn tohex [dense-hash]
  (apply str (map (fn [s] (if (= 1 (count s))
                            (str "0" s)
                            s))
                  (map (fn [n] (Integer/toHexString n)) dense-hash))))

(comment
  (tohex (dense (round64 (lengths str-input))))
)
