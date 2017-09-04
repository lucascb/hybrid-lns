(ns core
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

(defn to-neanderthal-matrix
  [matrix]
  (let [n (count matrix)
        values (flatten matrix)]
    (dge n n values {:order :row})))

;; Problem specs
;; x -> route matrix
;; d -> distance matrix
;; q -> max capacity of a route
;; c -> demands of each customer
;(def vrp (read-string (slurp "A-n32-k5.txt")))
(def vrp (p/parse-file "A-n32-k5.vrp"))
(def customers (range 2 (:dimension vrp)))
(def d (to-neanderthal-matrix (:distances vrp)))
(def q (:capacity vrp))
(def c (:demands vrp))

;; Test cases
(def z1 (build-route [21 31 19 17 13 7 26] d))
(def z2 (build-route [12 16 30] d))
(def z3 (build-route [27 24] d))
(def z4 (build-route [29 18 8 9 22 15 10 25 5 20] d))
(def z5 (build-route [14 28 11 23 3 6] d))
(def s [z1 z2 z3 z4 z5])

(def sol [[21 31 19 17 13 7 26] [12 1 16 30] [27 24] [29 18 8 9 22 15 10 25 5 20] [14 28 11 4 23 3 6]])

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
(defn can-add?
  "Checks if the customer i can be added to the route x
  given q: max capacity of a vehicle and c: demands of customers"
  [i x q c]
  (let [i-demand (nth c i)]
    (<= (+ i-demand (:cost x)) q)))

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
  "Insert the customer i in the position pos of the route x"
  [x i pos]
  (concat (take pos x) [i] (drop pos x)))

(defn insert-at-route
  "Insert the customer i on each position of the route x and returns the best pos
  given d: distance matrix and x the sequence which represents the route"
  [x i d]
  (let [new-routes (for [pos (range (inc (count x)))]
                     (insert-at-pos x i pos))]
    (println (first new-routes))
    (first (sort-by :cost (map #(build-route % d) new-routes)))))

(defn insert-at-best-route
  "Inserts the customer i in the first and second best possible route of the solution s, given d: distance matrix, q: max capacity of a vehicle, c: demands of customers"
  [s i d q c]
  (let [best-pos (for [x s :when (can-add? i x q c)]
                   (insert-at-route (:tour x) i d))
        best-routes (sort-by :cost best-pos)]
    [(first best-routes) (second best-routes)]))

(defn calculate-regrets
  "Calculates the reinsertion of each customers from the removed bank rc into its best routes of the solution s, given d: dist matrix, q: max capacity of a vehicle, c: demands of each customer"
  [rc s d q c]
  (for [i rc] ; for each customer in the removed bank
    (let [[first-best second-best] (insert-at-best-route s i d q c)
          delta (- (:cost second-best) (:cost first-best))]
      {:customer i :delta delta :route first-best})))

(defn add-to-solution
  "Adds the new route nx with the inserted customer ni to the solution s"
  [s ni nx]
  (let [old-x (remove #(= % ni) (:tour nx))]
    (println (keys nx))
    (cons nx (remove #(= (:tour %) old-x) s))))

(defn regret-2
  "Constructive heuristic to reinsert the customers removed from Worst Removal"
  [rc s d q c]
  (if (empty? rc) s
      (let [regrets (calculate-regrets rc s d q c)
            max-regret (first (sort-by :delta > regrets))
            chosen-cust (:customer max-regret)
            chosen-route (:route max-regret)]
        (println chosen-cust)
        (recur (remove #(= % chosen-cust) rc)
               (add-to-solution s chosen-cust chosen-route)
               d q c))))

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
