(ns quil-sketches.vector
  (:require [quil.core :as q]))

(defn x [v]
  (first v))

(defn y [v]
  (second v))

(defn mag [v]
  (q/sqrt (+ (* (x v)
                (x v))
             (* (y v)
                (y v)))))

(defn add [v1 v2]
  [(+ (x v1)
      (x v2))
   (+ (y v1)
      (y v2))])

(defn sub [v1 v2]
  [(- (x v1)
      (x v2))
   (- (y v1)
      (y v2))])

(defn mult [s v1]
  [(* s
      (x v1))
   (* s
      (y v1))])

(defn div [s v1]
  [(/ s
      (x v1))
   (/ s
      (y v1))])
