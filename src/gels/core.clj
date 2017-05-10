(ns gels.core
  (:gen-class))

(use 'clojure.core.matrix)
(use 'clojure.core.matrix.operators)
(set-current-implementation :vectorz)

(defn gels
  "Implementation of Gravitational Emulation Local Search"
  []
  (matrix [[1 2] [3 4]]))
