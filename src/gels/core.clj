(ns gels.core
  (:require [parser.core :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

(defn convert-matrix
  "Convert a list of lists to a Neanderthal Matrix"
  [m n]
  (dge n n (reduce concat m)))

(defn -main
  [args &]
  ("Hello World!"))
