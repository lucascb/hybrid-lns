(ns hybrid-lns
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native]))

;; Data structures
; A route consists in its matrix representation
(defstruct Route :matrix :cost :tour)
(defstruct Solution :routes :cost)

;; Global constants
; Algorithm parameters
(def PARAMS (read-string (slurp "hybrid_lns.params")))
(def INSTANCE-NAME (:instance PARAMS))
(def INSTANCE (read-string (slurp (str INSTANCE-NAME ".in"))))

; Instance variables
(def N (:dimension INSTANCE)) ; Number of customers
(def K (:no-of-trucks (:comment INSTANCE))) ; Number of vehicles available
(def Q (:capacity INSTANCE)) ; Maximum capacity of a vehicle
(def DIST (p/to-neanderthal-matrix (:distances INSTANCE))) ; Distance matrix
(def DEMS (:demands INSTANCE)) ; Demand of each customer

; Worst removal parameters
(def P (:wr-p PARAMS))
(def Q (+ (rand-int (min 96 (* 0.4 (- N 4)))) 4))

; ACO parameters
(def R1 (:aco-r1 PARAMS))
(def R2 (:aco-r2 PARAMS))
(def R3 (:aco-r3 PARAMS))
(def R1-R2 (+ R1 R2))
(def R1-R2-R3 (+ R1 R2 R3))
(def ALPHA (:aco-alpha PARAMS))
(def BETA (:aco-beta PARAMS))
(def PSI (:aco-psi PARAMS))
(def E (- 1 (:aco-e PARAMS))) ; Evaporation coefficient
(def MAX-ITER (:aco-niter PARAMS))

;; Solution utils
(defn feasible?
  "Returns if a solution is feasible or not"
  [sol]
  (let [routes (:routes sol)]
    (and (<= (count routes) K) ; There is at most k routes
         (every? true?           ; The total demand of each route does not exceed
                 (for [r routes] ; the vehicle capacity
                   (<= (reduce + (map #(nth DEMS %) (:tour r))) Q))))))

(defn total-cost
  "Returns the cost of a solution"
  [sol]
  (reduce + (map :cost sol)))

(defn build-solution
  "Return the solution and calculate its cost"
  [routes]
  (struct Solution routes (total-cost routes)))

;; Routes utils
(defn route-cost
  "Returns the cost of the route x"
  [x]
  (reduce + (for [i (range (mrows x))]
              (dot (row x i)
                   (row DIST i)))))

(defn build-route
  "Build the route and calculate its cost"
  [r]
  (let [path (map vector r (rest r))
        x (dge N N)]
    (entry! x 0 (first r) 1)
    (doseq [[i, j] path]
      (entry! x i j 1))
    (entry! x (last r) 0 1)
    (struct Route x (route-cost x) r)))

(defn empty-route
  "Returns an empty route"
  []
  (struct Route (dge N N) 0 []))

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

(defn build-route
  "Build the route and calculate its cost"
  [r]
  (let [path (map vector r (rest r))
        x (dge N N)]
    (entry! x 0 (first r) 1)
    (doseq [[i, j] path]
      (entry! x i j 1))
    (entry! x (last r) 0 1)
    (struct Route x (route-cost x) r)))

(defn empty-route
  "Returns an empty route"
  []
  (struct Route (dge N N) 0 []))

;; Generates the initial solution
(defn generate-route
  "Genereates a route x ramdomly from a list of available customers until the capacity is constrained"
  [cust route dem d q c]
  (if (empty? cust)
    (build-route route d)
    (let [chosen-cust (rand-nth cust)
          cust-dem (entry c chosen-cust)
          new-dem (+ dem cust-dem)]
      (if (> new-dem q)
        (build-route route d)
        (recur (remove #(= % chosen-cust) cust)
               (conj route chosen-cust)
               new-dem
               d q c)))))

(defn generate-initial-solution
  "Generates randomly a initial feasible solution with k routes"
  [k d q]
  ())

;; Removal heuristic
(defn- remove-customer
  "Removes the customer i from the route x"
  [x i]
  (let [nx (remove #(= % i) (:tour x))]
    (build-route nx)))

(defn- routes-without-customers
  "Generates routes without each customer and calculates its cost"
  [s]
  (flatten (for [x s]
             (for [i (:tour x)]
               (let [r (remove-customer x i)]
                 {:customer i
                  :delta-cost (- (:cost x) (:cost r))
                  :route r
                  :old-tour (:tour x)})))))

(defn- update-solution
  "Update the solution s to remove the customer chosen in worst-removal"
  [s rc]
  (conj (remove #(= (:tour %) (:old-tour rc)) s) (:route rc)))

(defn worst-removal
  "Removes randomly q worst costumers in the solution s"
  [s rb q]
  (if (= q 0) ; Returns the new solution (s) alongside with the
    [s rb]    ; customers removed in the removed bank (rb)
    (let [l (sort-by :delta-cost > (routes-without-customers s))
          y (rand)
          rc (nth l (int (* (Math/pow y P) (count l))))] ; Chosen customer to remove
      (recur (update-solution s rc)
             (conj rb (:customer rc))
             (dec q)))))

;; Insertion heuristic
(defn can-add?
  "Checks if the customer i can be added to the route x
  given q: max capacity of a vehicle and c: demands of customers"
  [i x]
  (let [i-demand (nth DEMS i)]
    (<= (+ i-demand (:cost x)) Q)))

(defn insert-at-pos
  "Insert the customer i in the position pos of the route x"
  [x i pos]
  (concat (take pos x) [i] (drop pos x)))

(defn insert-at-route
  "Insert the customer i on each position of the route x and returns the best pos"
  [x i]
  (let [new-routes (for [pos (range (inc (count x)))]
                     (insert-at-pos x i pos))]
    ;(println (first new-routes))
    (first (sort-by :cost (map build-route new-routes)))))

(defn insert-at-best-route
  "Inserts the customer i in the first and second best possible route of the solution"
  [s i]
  (let [best-pos (for [x s] ;:when (can-add? i x)
                   (insert-at-route (:tour x) i))
        best-routes (sort-by :cost best-pos)]
    (println "Routes with the customer: " i (map :tour best-routes) (map :cost best-routes))
    [(first best-routes) (second best-routes)]))

(defn calculate-regrets
  "Calculates the reinsertion of each customers from the removed bank rc into its best routes of the solution s"
  [rc s]
  (for [i rc] ; for each customer in the removed bank
    (let [[first-best second-best] (insert-at-best-route s i)
          delta (- (:cost second-best) (:cost first-best))]
      {:customer i :delta delta :route first-best})))

(defn add-to-solution
  "Adds the new route nx with the inserted customer ni to the solution s"
  [s ni nx]
  (let [old-x (remove #(= % ni) (:tour nx))]
    ;(println (keys nx))
    (cons nx (remove #(= (:tour %) old-x) s))))

(defn regret-2
  "Constructive heuristic to reinsert the customers rc removed from Worst Removal into the solution s"
  [rc s]
  (if (empty? rc) s
      (let [regrets (calculate-regrets rc s)
            max-regret (first (sort-by :delta > regrets))
            chosen-cust (:customer max-regret)
            chosen-route (:route max-regret)]
        (println "The customer " chosen-cust " is being inserted on the route " (:tour chosen-route))
        (recur (remove #(= % chosen-cust) rc)
               (add-to-solution s chosen-cust chosen-route)))))

;; Ant Colony Optimization
(defn build-heuristic-matrix
  "Build the heuristic matrix using the formula Sij = Di0 + D0j - Dij"
  []
  (let [s (dge N N)]
    (doseq [i (range N)]
      (doseq [j (range N)]
        (if (= i j)
          (entry! s i j 0)
          (entry! s i j (+ (entry DIST i 0)
                           (- (entry DIST 0 j)
                              (entry DIST i j)))))))
    s))

(defn build-pheromone-matrix
  "Build the initial pheromone trail matrix"
  [total-dist]
  (let [v (repeat (* N N) total-dist)]
    (dge N N v)))

(defn cross?
  "True if any of the solutions cross the path (i, j), nil otherwise"
  [sol i j]
  (some #(= (entry % i j) 1.0) (map :matrix (:routes sol))))

(defn evaporate
  "Update the pheromone matrix"
  [sol t-matrix]
  (let [cost (/ 1 (:cost sol))]
    ;(println "Cost: " cost)
    (doseq [i (range N)]
      (doseq [j (range N)]
        (let [deposit-amount (if (cross? sol i j) cost 0)
              pheromone (entry t-matrix i j)]
          (entry! t-matrix i j (+ (* E pheromone) deposit-amount)))))
    t-matrix))

(defn exploitation
  "Defines the next customer based on exploitation, given a: available customers, i: last customer added, t: the pheromone trail matrix, h: heuristic matrix"
  [a i t-matrix h-matrix]
  (let [exps (for [j a] {:cust j :val (* (Math/pow (entry t-matrix i j) ALPHA)
                                         (Math/pow (entry h-matrix i j) BETA))})]
    (println "Performing exploitation")
    (:cust (apply max-key :val exps))))

(defn build-probabilities
  "Builds the probabilities of each possible customer to be inserted"
  [a i t-matrix h-matrix]
  (let [x (reduce + (for [j a] (* (Math/pow (entry t-matrix i j) ALPHA)
                                  (Math/pow (entry h-matrix i j) BETA))))
        probs (for [j a] {:cust j :prob (/ (* (Math/pow (entry t-matrix i j) ALPHA)
                                              (Math/pow (entry h-matrix i j) BETA))
                                           x)})]
    (println "---------- Build probabilities ----------")
    (println probs)
    (println "-----------------------------------------")
    probs))

(defn build-roullette
  "Build a roullette based on the probabilities of each customer"
  [probs r acc]
  (if (empty? probs)
    (let []
      (println "----------- Build roullette ----------")
      (println r)
      (println "--------------------------------------")
      r)
    (let [x (first probs)
          cust (:cust x)
          prob (:prob x)
          p (+ acc prob)]
      (recur (rest probs)
             (conj r {:cust cust :val p})
             p))))

(defn spin-roullette
  "Spin the roullette and choose the next customer"
  [r drawn-num]
  (let [cur (first r)]
    (if (> (:val cur) drawn-num)
      (:cust cur)
      (recur (rest r) drawn-num))))

(defn biased-exploration
  "Defines the next customer based on biased exploration, given a: available customers, "
  [a i t-matrix h-matrix]
  (let [probs (build-probabilities a i t-matrix h-matrix)
        roullette (build-roullette probs [] 0)
        p (rand)]
    (println "Performing biased exploration")
    (spin-roullette roullette p)))

(defn random-selection
  "Defines the next customer randomly"
  [a]
  (println "Performing random selection")
  (rand-nth a))

(defn last-customer
  "Returns the last customer of a route"
  [x]
  (let [tour (:tour x)]
    (if (empty? tour) 0 (last tour))))

(defn add-to-route
  "Add costumer i to route x"
  [x i]
  (let [l (last-customer x)]
    (struct Route
            (entry! (:matrix x) l i 1.0) ; Route matrix
            (+ (:cost x) (entry DIST l i)) ; Route cost
            (conj (:tour x) i)))) ; Route sequence

(defn add-next-customer
  "Select the next customer to be added to the route x"
  [x cust t-matrix h-matrix]
  (let [i (last-customer x) ; Last customer added to the route
        r (rand)
        j (cond (<= r R1)
                (exploitation cust i t-matrix h-matrix)
                (and (< R1 r) (<= r R1-R2))
                ;(biased-exploration cust i t-matrix h-matrix)
                (exploitation cust i t-matrix h-matrix)
                (and (< R1-R2 r) (<= r R1-R2-R3))
                (random-selection cust))]
    (add-to-route x j)))

(defn generate-solution
  "Inserts the customers into the route i based on ACO"
  [sol cust i t-matrix h-matrix]
  (println "----------------------------------------")
  (println "Old route:" (:tour (nth sol i)))
  (if (empty? cust)
    (build-solution sol)
    (let [x (nth sol i)
          new-x (add-next-customer x cust t-matrix h-matrix)]
      (println "Solution:" (map :tour sol))
      (println "Route number:" i)
      (println "New route:" (:tour new-x))
      (println "Customers to add:" cust)
      (println "---------------------------------------")
      (recur (assoc sol i new-x)
             (remove #(= % (last-customer new-x)) cust)
             (mod (inc i) K)
             t-matrix
             h-matrix))))

(defn hybrid-lns
  "Genereates a solutions using Hybrid Large Neighborhood Search"
  [initial h t]
  (spit (str INSTANCE-NAME ".out")
        {:date (.format (java.text.SimpleDateFormat. "dd/MM/yyyy HH:mm:ss") (java.util.Date.))
         :solution (generate-solution initial (range 1 N) 0 h t)
         :params PARAMS}))

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
(defn start
  "Perfoms a Large Neighborhood Search on the solution s"
  [s best-s q p fi r1 r2 r3 alpha beta dist cap dem]
  (let [removed-bank (worst-removal s q dist p) ; Remove q customer from the solution
        new-s (regret-2 removed-bank s dist cap dem) ; Reinserts these customers
        size (ncols (:matrix (first s)))]
    (if (< (total-cost new-s) (total-cost s))
      (if (< (total-cost new-s) (total-cost best-s))
        (recur new-s new-s q p fi r1 r2 r3 alpha beta dist cap dem)
        (recur new-s best-s q p fi r1 r2 r3 alpha beta dist cap dem))
      (let [new-s (aco best-s (range 1 size) dist fi r1 r2 r3 alpha beta)]
        (if (< (total-cost new-s) (total-cost best-s))
          (recur new-s new-s q p fi r1 r2 r3 alpha beta dist cap dem)
          (recur new-s best-s q p fi r1 r2 r3 alpha beta dist cap dem))))))
