(ns gels.core
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native]))

;; Global variables
(def PARAMS (read-string (slurp "gels.params")))
(def INSTANCE (read-string (slurp (str (:instance PARAMS) ".in"))))

; Algorithm constants
(def G (:g PARAMS))
(def MAX-ITER (:max-iter PARAMS))
(def INITIAL-VELOCITY (:initial-velocity PARAMS))
(def MAX-VELOCITY (:max-velocity PARAMS))

; Problem specs
(def N (:dimension INSTANCE))
(def DIST (p/to-neanderthal-matrix (:distances INSTANCE)))

;; Utils
(defn convert-matrix
  "Convert a list of lists to a Neanderthal Matrix"
  [m n]
  (dge n n (reduce concat m)))

(defn mass
  "Calculate the mass of a costumer"
  [d v]
  (double (/ d v)))

(defn create-velocity-matrix
  "Create the velocity matrix"
  []
  (let [vel (dge N N)]
    (doseq [i (range N)]
      (doseq [j (range N)]
        (entry! vel i j INITIAL-VELOCITY)))
    vel))

(defn create-time-matrix
  "Create time matrix based on distance and velocity matrixes"
  [d s n]
  (convert-matrix n n
   (for [i (range n)]
     (for [j (range n)]
       (mass (d i j) (s i j))))))

(defn gravitational-force
  "Calculate the gravitational force between two corpses"
  [cu ca r]
  (double (/ (* G (- cu ca)) (* r r))))

;; Definitions
; Distance matrix
(def dists (convert-matrix (:distances bench)))
(def size (:dimension bench))
; Velocity matrix
(def vel (dge size size 100.0))
; Time matrix
(def time (create-time-matrix dists vel size))

(defn gels
  "Implementation of Gravitational Emulation Local Search"
  []
  ("oi"))

(defn -main
  [args &]
  ("Hello World!"))
