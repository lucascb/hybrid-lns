(ns core
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

;; Problem specs
;; x -> route matrix
;; d -> distance matrix
;; q -> max capacity of a route
;(def vrp (read-string (slurp "A-n32-k5.txt")))
(def vrp (p/parse-file "A-n32-k5.vrp"))
(def customers (range 2 (:dimension vrp)))
(def dist (:distances vrp))

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

(defn finished?
  "True if there is any customer available, or false otherwise"
  [cust]
  (= (int (sum cust)) (dec (dim cust))))

(defn choose-customer
  "Chooses randomly a customer and return it"
  [cust]
  (if (finished? cust)
    -1
    (let [chosen-cust (rand-nth (range 1 (dim cust)))]
      (if (= (entry cust chosen-cust) 0.0)
        chosen-cust
        (recur cust)))))

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
(defn build-route
  "Build the route and calculate its cost"
  [route d]
  (let [n (ncols d)
        path (map vector route (rest route))
        x (dge n n)
        pos 0]
    (entry! x 0 (first route) 1)
    (doseq [[i, j] path :let [pos (inc pos)]]
      (println pos)
      (entry! x i j 1))
    (entry! x (last route) 0 1)
    {:tour x :cost (route-cost x d)}))

(defn insert-at-best-pos
  "Inserts the customer c into its best position in route x"
  [x c d]
  (let [fst-best nil sec-best nil]
    (for [i (range (ncols x))]
      (if (< (cost-at-pos x i c)
             fst-best-pos)
        (:cost cost-at-pos :route (set! fst-best-pos i))))))

(defn insert-at-best-routes
  "Returns the routes and cost for customer c to be inserted in its best positions"
  [s c]
  (for [i (range (count s))]
    (insert-at-best-pos (nth s i) c)))

(defn calculate-regret
  "Calculates the reinsertion of the customers rc into its best routes"
  [rc s]
  (for [cust rc] ; for each customer in the removed bank
    (let [routes (sort-by :cost (insert-at-best-routes cust s))
          first-best (first routes)
          second-best (second routes)
          delta (- second-best first-best)]
      {:cust cust :delta delta :sol first-best})))

(defn regret-2
  "Constructive heuristic to reinsert the customers removed from Worst Removal"
  [rc s]
  (if (empty? rc)
    s
    (let [regrets (map #(calculate-regret % s) rc)
          max-regret (first (sort-by :delta > regrets))
          chosen-cust (:cust max-regret)]
      (add-to-solution chosen-cust (:route (:sol max-regret)) s)
      (recur (remove #(= % chosen-cust) rc)
             s))))

;; Ant Colony Optimization
(def aco
  "Perform an Ant Colony Optimization on a solutions to improve it"
  [s d r1 r2 r3 fi]
  (let [size (ncols s)
        t (dge size size 1.0) ; Pheromones matrix
        n (dge size size 0)]  ; Heuristic matrix
    (for [])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
