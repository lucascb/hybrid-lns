(ns core
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

;; Problem specs
(def vrp (read-string (slurp "A-n32-k5.txt")))
(def customers (range 2 (:dimension vrp)))

;; Utils
(defn int->key
  "Converts an integer to a hashmap keyword"
  [v]
  (keyword (str v)))

(defn demand
  "Gets the demand of a customer"
  [cust]
  ((int->key cust) (:demands vrp)))

(defn in?
  "Checks if an element belongs to a collection"
  [elem coll]
  (some #(= elem %) coll))

(defn at
  "Equivalent to m[i][j]"
  [m i j]
  (nth (nth m i) j))

(defn cost
  "Returns the cost of a tour"
  [dist-matrix tour]
  (reduce + (apply dot )))

(defn calculate-load
  "Calculates the total load of a tour"
  [tour]
  (reduce + (map demand tour)))

;; Generates the initial solution
(defn generate-tour
  [customers dists last tour cap]
  (let [chosen-cust (rand-nth customers)]
    (if (> (+ (cost tour)
              (entry dists last chosen-cust)
              (entry dists chosen-cust 0))
            cap)
      (entry! tour last 0 1)
      (recur (remove #(= % chosen-cust) customers)
             dists
             chosen-cust
             (entry! tour last chosen-cust 1)
             cap))))

(defn generate-initial-solution
  "Generates randomly a initial feasible solution with k vehicles"
  [n k d c]
  (for [i (range k)]
    (generate-tour (range n) d 0 (dge n n) c)))

;; Heuristica de remocao
(defn worst-removal
  "Removes randomly the worst costumer in the tour"
  []
  ())

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
