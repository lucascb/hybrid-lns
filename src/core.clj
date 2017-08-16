(ns core
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

;; Problem specs
;; x -> route matrix
;; d -> distance matrix
;; q -> max capacity of a route
(def vrp (read-string (slurp "A-n32-k5.txt")))
(def customers (range 2 (:dimension vrp)))

;; Test cases
(def d (dge 3 3 [0 3 3 3 0 2 3 2 0] {:order :row}))
(def r (dge 3 3 [0 1 0 0 0 1 1 0 0] {:order :row}))

;; Utils
(defn route-cost
  "Returns the cost of the route x"
  [x d]
  (reduce + (for [i (range (mrows x))]
              (dot (row x i)
                   (row d i)))))

(defn choose-customer
  "Chooses randomly a customer and return it"
  [cust]
  (if (finished? cust)
    -1
    (let [chosen-cust (rand-nth (range 1 (dim cust)))]
      (if (= (entry cust chosen-cust) 0.0)
        chosen-cust
        (recur cust)))))

(defn finished?
  "True if there is any customer available, or false otherwise"
  [cust]
  (= (int (sum cust)) (dec (dim cust))))

;; Generates the initial solution
(defn generate-route
  "Generates a route x randomly from a list of available customers with distances d for a vehicle with capacity q"
  [cust last x d q]
  (if (finished? cust)
    (entry! x last 0 1)
    (let [chosen-cust (choose-customer cust)]
      (println last chosen-cust)
      (if (> (+ (route-cost x d)
                (entry d last chosen-cust)
                (entry d chosen-cust 0))
             q)
        (entry! x last 0 1)
        (recur (entry! cust chosen-cust 1)
               chosen-cust
               (entry! x last chosen-cust 1)
               d
               q)))))

(defn generate-initial-solution
  "Generates randomly a initial feasible solution with k routes"
  [k d q]
  (let [n (ncols d)
        cust (dv n)]
    (for [i (range k)]
      (generate-route cust 0 (dge n n) d q))))

;; Removal heuristic
(defn cost-without-customer
  "Removes the customer i from the route and recalculates the cost"
  [route i]
  ("oi"))

(defn worst-removal
  "Removes randomly the worst costumer in the tour"
  []
  ("oi"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
