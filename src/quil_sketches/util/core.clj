(ns quil-sketches.util.core)

(defn point-coll [point e]
  (and (> (:x point)
          (:x e))
       (> (:y point)
          (:y e))
       (< (:x point)
          (+ (:x e)
             (:w e)))
       (< (:y point)
          (+ (:y e)
             (:h e)))))

(defn chance?
  [p _]
  (< (rand 1)
     p))

(defn if-r [p e & [f]]
  (if (< (rand 1)
         p)
    e
    f))

;added this so
(defn random
  [min max]
  (if (>= min max)
    min
    (+ min (rand (- max min))) ))

(defn random-int
  [min max]
  (int (random min max)))

(defn random-seq
  [min max]
  (repeatedly #(random min max)))

(defn grid
  ([w h sx sy]
   (grid 0 0 w h sx sy))
  ([sx sy w h step-x step-y]
   (for [x (range sx w step-x)
         y (range sy h step-y)]
     [x y])))

(defn random-grid
  [r-args1 r-args2 grid]
  (map (fn [[x y]]
         [(+ x (apply random r-args1))
          (+ y (apply random r-args2))])
       grid))
