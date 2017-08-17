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

(defn remove-customer
  "Removes the customer c from a route x"
  [x c]
  (doseq [i (range (ncols x))]
    (entry! x i c 0)
    (entry! x c i 0)))

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
  "Removes the customer c from the route and recalculates the cost"
  [x c d]
  (let [y (dge x)]
    (doseq [i (range (mrows y))]
      (entry! y i c 0)
      (entry! y c i 0))
    (println y)
    (- (route-cost x d) (route-cost y d))))

(defn routes-without-customers
  "Generates routes without each customer and calculates its cost"
  [x d]
  (for [i (range 1 (mrows x))]
    {:customer i :cost (cost-without-customer x i d)}))

(defn worst-removal
  "Removes randomly the worst costumer in the tour"
  [x d p]
  (let [l (sort-by :cost > (routes-without-customers x d))
        y (rand)
        r (nth l (int (Math/floor (* (Math/pow y p) (count l)))))]
    (remove-customer x r)
    r))

;; Insertion heuristic
(defn insert-at-best-pos
  "Inserts the customer c into its best position in route x"
  [x c]
  (let [fst-best-pos 0
        fst-best-cost 100000
        sec-best-pos 0
        sec-best-cost 100000]
    (for [i (range (ncols x))]
      (if (< (cost-at-pos x i c)
             fst-best-pos)
        (do (set! fst-best-pos i)))))

  (defn first-best-route
    "Returns the best route for customer c to be inserted in its best position"
    [s c]
    (for [i (range (count s))]
      (insert-at-best-pos (nth s i) c))))

(defn regret-2
  "Reinserts the customers c into its best routes"
  [c s]
  ())

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
