(ns core
  (:require [parser :as p]
            [hybrid-lns :as lns])
  (:gen-class))

;; Problem specs
;; x -> route matrix
;; d -> distance matrix
;; q -> max capacity of a route
;; c -> demands of each customer
(def vrp (p/parse-file "A-n32-k5.vrp"))
(def customers (range 2 (:dimension vrp)))
(def d (p/to-neanderthal-matrix (:distances vrp)))
(def q (:capacity vrp))
(def c (:demands vrp))

;; Test case
(def z1 (lns/build-route [21 31 19 17 13 7 26] d))
(def z2 (lns/build-route [12 16 30] d))
(def z3 (lns/build-route [27 24] d))
(def z4 (lns/build-route [29 18 8 9 22 15 10 25 5 20] d))
(def z5 (lns/build-route [14 28 11 23 3 6] d))
(def s [z1 z2 z3 z4 z5])

(def size (:dimension vrp))

(def h (lns/build-heuristic-matrix d))
(def t (lns/build-pheromone-matrix size))
(def x (lns/empty-route d))

(def r1 0.5)
(def r2 0.3)
(def r3 0.2)

(defn -main
  "Test"
  [args &]
  "Hello world!")
