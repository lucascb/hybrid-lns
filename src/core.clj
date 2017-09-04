(ns core
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

(defn to-neanderthal-matrix
  [matrix]
  (let [n (count matrix)
        values (reduce concat matrix)]
    (dge n n values {:order :row})))

;; Problem specs
;; x -> route matrix
;; d -> distance matrix
;; q -> max capacity of a route
;(def vrp (read-string (slurp "A-n32-k5.txt")))
(def vrp (p/parse-file "A-n32-k5.vrp"))
(def customers (range 2 (:dimension vrp)))
(def dist (to-neanderthal-matrix (:distances vrp)))

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
        x (dge n n)]
    (entry! x 0 (first route) 1)
    (doseq [[i, j] path]
      (entry! x i j 1))
    (entry! x (last route) 0 1)
    {:matrix x :cost (route-cost x d) :tour route}))

(defn insert-at-pos
  "Insert the customer c in the position i of the route x"
  [x c i]
  (concat (take i x) [c] (drop i x)))

(defn insert-at-route
  "Insert the customer c on each position of the route x and returns the best pos"
  [x c d]
  (let [new-routes (for [i (range (inc (count x)))] (insert-at-pos x c i))]
    (first (sort-by :cost (map #(build-route % d) new-routes)))))

(defn insert-at-best-route
  "Inserts the customer c in the best possible route of the solution s"
  [s c d]
  (let [best-pos (for [x s] (insert-at-route x c d))]
    (first (sort-by :cost best-pos))))

(defn calculate-regret
  "Calculates the reinsertion of the customers rc into its best routes of the solution s"
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
      ;(add-to-solution chosen-cust (:route (:sol max-regret)) s)
      (recur (remove #(= % chosen-cust) rc)
             s))))

;; Ant Colony Optimization
(defn aco
  "Perform an Ant Colony Optimization on a solutions to improve it"
  [s d r1 r2 r3 fi]
  (let [size (ncols s)
        t (dge size size 1.0) ; Pheromones matrix
        n (dge size size 0)]  ; Heuristic matrix
  ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
