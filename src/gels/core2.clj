(ns gels.core
  (:require [parser.core :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

;; Constants
(def G 6.672)

;; Utils
(defn convert-matrix
  "Convert a list of lists to a Neanderthal Matrix"
  [m n]
  (dge n n (reduce concat m)))

(defn mass
  "Calculate the mass of a costumer"
  [d v]
  (double (/ d v)))

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

; Benchmark parsed
(def bench (p/parse-file "A-n32-k5.vrp"))
; Distance matrix
(def dists (convert-matrix (:distances bench)))
(def size (:dimension bench))
; Velocity matrix
(def vel (dge size size 100.0))
; Time matrix
(def time (create-time-matrix dists vel size))
; Maximum velocity
(def max-vel 100)
; Maximum number of iterations
(def max-iter 100)

(defn gels
  "Implementation of Gravitational Emulation Local Search"
  []
  ("oi"))

(defn -main
  [args &]
  ("Hello World!"))
