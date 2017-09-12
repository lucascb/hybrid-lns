(ns core
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

;; Problem specs
;; x -> route matrix
;; d -> distance matrix
;; q -> max capacity of a route
;; c -> demands of each customer
;(def vrp (read-string (slurp "A-n32-k5.txt")))
(def vrp (p/parse-file "A-n32-k5.vrp"))
(def customers (range 2 (:dimension vrp)))
(def d (p/to-neanderthal-matrix (:distances vrp)))
(def q (:capacity vrp))
(def c (:demands vrp))

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
      ;(println last chosen-cust)
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
  [x q d p]
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
    ;(println (first new-routes))
    (first (sort-by :cost (map #(build-route % d) new-routes)))))

(defn insert-at-best-route
  "Inserts the customer i in the first and second best possible route of the solution s, given d: distance matrix, q: max capacity of a vehicle, c: demands of customers"
  [s i d q c]
  (let [best-pos (for [x s] ;:when (can-add? i x q c)
                   (insert-at-route (:tour x) i d))
        best-routes (sort-by :cost best-pos)]
    (println "Routes with the customer: " i (map :tour best-routes) (map :cost best-routes))
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
    ;(println (keys nx))
    (cons nx (remove #(= (:tour %) old-x) s))))

(defn regret-2
  "Constructive heuristic to reinsert the customers removed from Worst Removal"
  [rc s d q c]
  (if (empty? rc) s
      (let [regrets (calculate-regrets rc s d q c)
            max-regret (first (sort-by :delta > regrets))
            chosen-cust (:customer max-regret)
            chosen-route (:route max-regret)]
        (println "The customer " chosen-cust " is being inserted on the route " (:tour chosen-route))
        (recur (remove #(= % chosen-cust) rc)
               (add-to-solution s chosen-cust chosen-route)
               d q c))))

;; Ant Colony Optimization
(defn build-heuristic-matrix
  "Build the heuristic matrix using the formula Sij = Di0 + D0j - Dij"
  [d]
  (let [n (ncols d)
        s (dge n n)]
    (doseq [i (range 1 n)]
      (doseq [j (range 1 n)]
        (if (= i j)
          (entry! s i j 0)
          (entry! s i j (+ (entry d i 0)
                           (- (entry d 0 j)
                              (entry d i j)))))))
    s))

(defn exploitation
  "Defines the next customer based on exploitation, given a: available customers, i: last customer added, t: the pheromone trail matrix, n: heuristic matrix, alpha and beta algorithm parameters"
  [a i t n alpha beta]
  (let [exps (for [j a] {(keyword (str j)) (* (Math/pow (entry t i j) alpha)
                                              (Math/pow (entry n i j) beta))})]
    (key (apply max-key val exps))))

(defn build-probabilities
  "Builds the probabilities of each possible customer to be inserted"
  [a i t n alpha beta]
  (let [x (reduce + (for [j a] (* (Math/pow (entry t i j) alpha)
                                  (Math/pow (entry n i j) beta))))
        probs (for [j a] {(keyword (str j)) (/ (* (Math/pow (entry t i j) alpha)
                                                  (Math/pow (entry n i j) beta))
                                               x)})]
    (reduce into probs)))

(defn build-roullette
  "Build a roullette based on the probabilities of each customer"
  [probs r acc]
  (if (empty? probs)
    [r acc]
    (let [[cust prob] (first probs)
          p (+ acc prob)]
      (recur (rest probs)
             (conj r {cust p})
             p))))

(defn spin-roullette
  "Spin the roullette and chooses the next customer"
  [r drawn-num]
  (if (> (vals (second r)) drawn-num)
    (first r)
    (recur (rest r) drawn-num)))

(defn biased-exploration
  "Defines the next customer based on biased exploration, given a: available customers, "
  [a i t n alpha beta]
  (let [probs (build-probabilities a i t n alpha beta)
        [roullette max-prob] (build-roullette probs [] 0)
        p (rand max-prob)
        j (spin-roullette roullette p)]
    j))

(defn random-selection
  "Defines the next customer randomly"
  [a]
  (rand-nth a))

(defn last-customer
  "Returns the last customer of a route"
  [x]
  (let [tour (:tour x)]
    (if (empty? tour) 0 (last tour))))

(defn add-to-route
  "Add costumer i to route x"
  [x i d]
  (let [l (last-customer x)]
    {:matrix (entry! (:matrix x) l i 1)
     :tour (conj (:tour x) i)
     :cost (+ (:cost x) (entry d l i))}))

(defn add-next-customer
  "Perform an Ant Colony Optimization on a solutions to improve it"
  [x a d t n r1 r2 r3 alpha beta]
  (if (empty? a)
    x
    (let [size (ncols (:matrix x))
          i (last-customer x) ; Last customer added to the route
          r (rand)
          j (cond (<= r r1)
                  (exploitation a i t n alpha beta)
                  (and (< r1 r) (<= r r2))
                  (biased-exploration a i t n alpha beta)
                  (and (<= r2 r) (<= r r3))
                  (random-selection a))]
      (add-to-route x j d))))

(defn aco
  "Perform an Ant Colony Optimization and returns "
  [old-s a d fi r1 r2 r3 alpha beta]
  (if (some #(> (count (:tour %)) fi) old-s)
    (let [size (ncols (first old-s))
          new-s (for [x old-s] (dge size size))]
      (println "--- Restarting ACO ---")
      (recur new-s (range 1 size) d fi r1 r2 r3 alpha beta))
    (let [k (count old-s) ; Number of routes of a solution
          size (ncols (first old-s))
          t (dge size size 1.0) ; Pheromones trail matrix
          n (build-heuristic-matrix d) ; Heuristic matrix
          new-s (for [x old-s] (add-next-customer x a d t n r1 r2 r3 alpha beta))]
      (println (map :tour new-s))
      (recur new-s a d fi r1 r2 r3 alpha beta))))

;; Large Neighborhood Search
(defn objec-func
  "Returns the objetive value of a solution s"
  [s]
  (reduce + (map :cost s)))

(defn lns
  "Perfoms a Large Neighborhood Search on the solution s"
  [s best-s q p fi r1 r2 r3 alpha beta dist cap dem]
  (let [removed-bank (worst-removal s q dist p) ; Remove q customer from the solution
        new-s (regret-2 removed-bank s dist cap dem) ; Reinserts these customers
        size (ncols (:matrix (first s)))]
    (if (< (objec-func new-s) (objec-func s))
      (if (< (objec-func new-s) (objec-func best-s))
        (recur new-s new-s q p fi r1 r2 r3 alpha beta dist cap dem)
        (recur new-s best-s q p fi r1 r2 r3 alpha beta dist cap dem))
      (let [new-s (aco best-s (range 1 size) dist fi r1 r2 r3 alpha beta)]
        (if (< (objec-func new-s) (objec-func best-s))
          (recur new-s new-s q p fi r1 r2 r3 alpha beta dist cap dem)
          (recur new-s best-s q p fi r1 r2 r3 alpha beta dist cap dem))))))

;; Test cases
(def z1 (build-route [21 31 19 17 13 7 26] d))
(def z2 (build-route [12 16 30] d))
(def z3 (build-route [27 24] d))
(def z4 (build-route [29 18 8 9 22 15 10 25 5 20] d))
(def z5 (build-route [14 28 11 23 3 6] d))
(def s [z1 z2 z3 z4 z5])

(def sol [[21 31 19 17 13 7 26] [12 1 16 30] [27 24] [29 18 8 9 22 15 10 25 5 20] [14 28 11 4 23 3 6]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
