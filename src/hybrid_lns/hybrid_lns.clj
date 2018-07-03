(ns hybrid-lns.hybrid-lns
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [uncomplicate.neanderthal core native]
        [random-seed.core]))

;; Hybrid large neighbourhood search algorithm for capacitated vehicle routing problem
;; Author: Akpinar [2016]

(def aco-initial-solution {:routes [] :cost Integer/MAX_VALUE})

;; Neanderthal conversions
(defn to-neanderthal-matrix
  "Converts a Clojure list of lists to a neanderthal matrix"
  [matrix]
  (let [n (count matrix)
        values (flatten matrix)]
    (dge n n values {:order :row})))

;; Data structures
; A route consists in its matrix representation
(defstruct Route :matrix :cost :tour)
(defstruct Solution :routes :cost)

;; Set global constants
(defn set-instance
  "Set the instance to be solved"
  [instance]
  (def INSTANCE instance)
  (def N (:dimension instance)) ; Number of customers
  (def K (:vehicles instance)) ; Number of vehicles available
  (def Q (:capacity instance)) ; Max capacity of a vehicle
  (def DIST (to-neanderthal-matrix (:distances instance))) ; Distance matrix
  (def DEMS (map second (:demand-section instance))) ; Demand of each customer
  (def NUM-EVALUATIONS (atom 0))
  (def WR-Q (+ (rand-int (min 96 (* 0.4 (- N 4)))) 4))
  (def OPTIMAL (:cost (:optimal INSTANCE) (:best-known INSTANCE 0))))

(defn set-parameters
  "Set the algorithm parameters"
  [params]
  (def PARAMS params)
  (println "Maximum number of tries =" @(def LNS-MAX-ITER (:max-iter params)))
  (println "Seed =" @(def SEED (:seed params)))
  ; Worst removal parameters
  (println "Worst-removal P =" @(def WR-P (:wr-p params)))
  ; ACO parameters
  (println "r1 ACO =" @(def ACO-R1 (:aco-r1 params)))
  (println "r2 ACO =" @(def ACO-R2 (:aco-r2 params)))
  (println "r3 ACO =" @(def ACO-R3 (:aco-r3 params)))
  (def ACO-R1-R2 (+ ACO-R1 ACO-R2))
  (println "alpha ACO =" @(def ACO-ALPHA (:aco-alpha params)))
  (println "beta ACO =" @(def ACO-BETA (:aco-beta params)))
  (println "psi ACO =" @(def ACO-PSI (:aco-psi params)))
  (println "P ACO =" @(def ACO-P (- 1 (:aco-p params)))) ; Evaporation coefficient
  (println "Max iterations ACO =" @(def ACO-MAX-ITER (:aco-iter params)))
  (println "Number of ants ACO =" @(def ACO-NUM-ANTS (:aco-ants params)))
  (set-random-seed! SEED))

;; Solution utils
(defn feasible?
  "Returns if a solution is feasible or not"
  [sol]
  (let [routes (:routes sol)]
    (every? true?           ; The total demand of each route does not exceed
            (for [r routes] ; the vehicle capacity
              (<= (reduce + (map #(nth DEMS %) (:tour r))) Q)))))

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
  (swap! NUM-EVALUATIONS inc)
  (reduce + (for [i (range (mrows x))]
              (dot (row x i)
                   (row DIST i)))))

(defn empty-route
  "Returns an empty route"
  []
  (struct Route (dge N N) 0 []))

(defn build-route
  "Build the route and calculate its cost"
  [r]
  (if (empty? r)
    (empty-route)
    (let [path (map vector r (rest r))
          x (dge N N)]
      (entry! x 0 (first r) 1)
      (doseq [[i, j] path]
        (entry! x i j 1))
      (entry! x (last r) 0 1)
      (struct Route x (route-cost x) r))))

(defn copy-route
  "Create a new route x' based on a route x"
  [x]
  (struct Route (dge (:matrix x)) (:cost x) (:tour x)))

(defn copy-solution
  "Create a new solution s' based on a solution s"
  [s]
  (struct Solution (vec (for [x (:routes s)] (copy-route x))) (:cost s)))

(defn empty-solution
  "Returns an empty solution"
  []
  (vec (repeat K [])))

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

;; Generate initial solution ramdomly
(defn invalid-route?
  "Check if `r` is invalid"
  [r]
  (every? zero? r))

(defn- split-routes
  "Split the tour `t` into routes"
  [t]
  (->> t
       (partition-by zero?)
       (remove invalid-route?)
       vec))

(defn generate-random-tour
  ""
  [customers tour load-so-far]
  (if (empty? customers)
    tour
    (let [cust (rand-nth customers)
          new-load (+ (nth DEMS cust) load-so-far)]
      (if (<= new-load Q)
        (recur (remove #{cust} customers)
               (conj tour cust)
               new-load)
        (recur (remove #{cust} customers)
               (conj tour 0 cust)
               (nth DEMS cust))))))

(defn generate-random-solution
  "Generate a random solution"
  []
  (let [;;depots (repeat (dec K) 0)
        ;;custs (range 1 N)
        ;;tour (shuffle (concat depots custs))
        tour (generate-random-tour (range 1 N) [] 0)
        routes (split-routes tour)]
    (build-solution (map build-route routes))))

;; Generate initial solution using Savings Heuristic
(defn merge-routes
  "Merge two routes on vertices i,j or return nil if the two routes cant be merged"
  [route1 route2 i j]
  (let [a (:tour route1)
        b (:tour route2)
        la (:load route1)
        lb (:load route2)]
    (cond (and (= i (last a)) (= j (first b)))
          {:tour (concat a b) :load (+ la lb)}
          (and (= i (first a)) (= j (first b)))
          {:tour (concat (reverse b) a) :load (+ la lb)}
          (and (= i (last a)) (= j (last b)))
          {:tour (concat a (reverse b)) :load (+ la lb)}
          (and (= i (first a) (= j (last b))))
          {:tour (concat b a) :load (+ la lb)})))

(defn enough-capacity?
  "Check whether the merge violates the capacity constrain of the vehicle"
  [a b]
  (<= (+ (:load a) (:load b)) Q))

(defn customer-route
  "Return the route which serves the customer i"
  [routes i]
  (let [r (first routes)
        t (:tour r)]
    (if (some #(= % i) t)
      r
      (recur (rest routes) i))))

(defn remove-routes
  "Remove the routes r1 and r2 from the list of routes"
  [routes r1 r2]
  (remove #(= % r2)
          (remove #(= % r1) routes)))

(defn calculate-savings
  "Calculate the Savings value between each pair of nodes"
  []
  (flatten
   (for [i (range 1 N)]
    (for [j (range 1 N) :when (< i j)]
      {:from i :to j :saving (- (+ (entry DIST i 0)
                                   (entry DIST 0 j))
                                (entry DIST i j))}))))

(defn generate-initial-solution
  ""
  [savings routes]
  ;(println "------ ROUTES ------")
  ;(println routes)
  (if (empty? savings)
    routes
    (let [saving (first savings)
          i (:from saving)
          j (:to saving)
          ri (customer-route routes i)
          rj (customer-route routes j)
          merge (merge-routes ri rj i j)]
      ;(println "Merging" ri "+" rj "=" merge)
      (if (and (not= ri rj) (enough-capacity? ri rj) merge)
        (recur (rest savings)
               (conj (remove-routes routes ri rj) merge))
        (recur (rest savings)
               routes)))))

(defn savings-heuristic
  ""
  []
  (let [savings (sort-by :saving > (calculate-savings))
        routes (for [i (range 1 N)] {:tour [i] :load (nth DEMS i)})]
    ;(println "****** SAVINGS ******")
    ;(println savings)
    (build-solution
     (map build-route (map :tour (generate-initial-solution savings routes))))))

;; Removal heuristic
(defn remove-customer
  "Removes the customer i from the route x"
  [x i]
  (let [nx (remove #(= % i) (:tour x))]
    (build-route nx)))

(defn routes-without-customers
  "Generates routes without each customer and calculates its cost"
  [s]
  (flatten (for [i (range (count s)) :let [x (nth s i)]]
             (for [c (:tour x)]
               (let [r (remove-customer x c)]
                 {:route-id i
                  :customer c
                  :delta-cost (- (:cost x) (:cost r))
                  :new-route r})))))

(defn update-solution
  "Update the solution s to remove the customer chosen in worst-removal"
  [s r]
  (assoc s (:route-id r) (:new-route r)))

(defn remove-randomly
  "Removes randomly q worst costumers in the solution s to the removed bank rb"
  [s rb q]
  ;(println "Solution:" (map :tour s))
  ;(println "Removed bank:" rb)
  (if (= q 0) ; Returns the new solution (s) alongside with the
    [(build-solution s) rb]    ; customers removed in the removed bank (rb)
    (let [l (sort-by :delta-cost > (routes-without-customers s))
          y (rand)
          i (int (* (Math/pow y WR-P) (count l)))
          ;_ (println "i:" i "l:" l)
          c (nth l i)]
          ;_ (println "c:" (:customer c))] ; Chosen customer to remove
      (recur (update-solution s c)
             (conj rb (:customer c))
             (dec q)))))

(defn worst-removal
  "Apply the Worst Removal heuristic to remove q customers from the solution"
  [s]
  (remove-randomly s [] WR-Q))

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
    ;(println "Routes with the customer: " i (map :tour best-routes) (map :cost best-routes))
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

(defn regret-2-insertion
  "Constructive heuristic to reinsert the customers rc removed from Worst Removal into the solution s"
  [rc s]
  (if (empty? rc)
    (build-solution s)
    (let [regrets (calculate-regrets rc s)
          max-regret (first (sort-by :delta > regrets))
          chosen-cust (:customer max-regret)
          chosen-route (:route max-regret)]
          ;(println "The customer " chosen-cust " is being inserted on the route " (:tour chosen-route))
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
  (some #(= (entry % i j) 1.0) sol))

(defn release-pheromone
  "Update the pheromone matrix"
  [t-matrix sol]
  (let [cost (/ 1000 (:cost sol))
        routes (map :matrix (:routes sol))]
    ;(println "Cost: " cost)
    (doseq [i (range N)]
      (doseq [j (range N)]
        (let [deposit-amount (if (cross? routes i j) cost 0)
              pheromone (entry t-matrix i j)]
          (entry! t-matrix i j (+ pheromone deposit-amount)))))
    t-matrix))

(defn evaporate
  "Evaporate the pheromone matrix based on the parameter P"
  [t-matrix]
  (doseq [i (range N)]
    (doseq [j (range N)]
      (let [pheromone (entry t-matrix i j)]
        (entry! t-matrix i j (* ACO-P pheromone)))))
  t-matrix)

(defn exploitation
  "Defines the next customer based on exploitation, given a: available customers, i: last customer added, t: the pheromone trail matrix, h: heuristic matrix"
  [a i t-matrix h-matrix]
  (let [exps (for [j a] {:cust j :val (* (Math/pow (entry t-matrix i j) ACO-ALPHA)
                                         (Math/pow (entry h-matrix i j) ACO-BETA))})]
    ;(println "Performing exploitation")
    (:cust (apply max-key :val exps))))

(defn build-probabilities
  "Builds the probabilities of each possible customer to be inserted"
  [a i t-matrix h-matrix]
  (let [x (reduce + (for [j a] (* (Math/pow (entry t-matrix i j) ACO-ALPHA)
                                  (Math/pow (entry h-matrix i j) ACO-BETA))))
        probs (for [j a] {:cust j :prob (/ (* (Math/pow (entry t-matrix i j) ACO-ALPHA)
                                              (Math/pow (entry h-matrix i j) ACO-BETA))
                                           x)})]
    ;(println "---------- Build probabilities ----------")
    ;(println probs)
    ;(println "-----------------------------------------")
    probs))

(defn build-roullette
  "Build a roullette based on the probabilities of each customer"
  [probs r acc]
  (if (empty? probs)
    (let []
      ;(println "----------- Build roullette ----------")
      ;(println r)
      ;(println "--------------------------------------")
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
  (if (= (count r) 1)
    (:cust (first r))
    (let [cur (first r)]
      (if (> (:val cur) drawn-num)
        (:cust cur)
        (recur (rest r) drawn-num)))))

(defn biased-exploration
  "Defines the next customer based on biased exploration, given a: available customers, "
  [a i t-matrix h-matrix]
  (let [probs (build-probabilities a i t-matrix h-matrix)
        roullette (build-roullette probs [] 0)
        p (rand)]
    ;(println "Performing biased exploration")
    (spin-roullette roullette p)))

(defn random-selection
  "Defines the next customer randomly"
  [a]
  ;(println "Performing random selection")
  (rand-nth a))

(defn last-customer
  "Returns the last customer of a route"
  [x]
  (if (empty? x) 0 (last x)))

(defn add-next-customer
  "Select the next customer to be added to the route x"
  [x cust t-matrix h-matrix]
  (let [i (last-customer x) ; Last customer added to the route
        r (rand)
        j (cond (<= r ACO-R1)
                (exploitation cust i t-matrix h-matrix)
                (and (< ACO-R1 r) (<= r ACO-R1-R2) (not= i 0))
                (biased-exploration cust i t-matrix h-matrix)
                :else
                (random-selection cust))]
    (conj x j)))

(defn generate-solution
  "Inserts the customers into the route i based on ACO"
  [routes cust i t-matrix h-matrix]
  ;(println "----------------------------------------")
  ;(println "Old route:" (:tour (nth sol i)))
  (if (empty? cust)
    (build-solution (map build-route routes))
    (let [x (nth routes i)
          new-x (add-next-customer x cust t-matrix h-matrix)]
      ;(println "Solution:" (map :tour sol))
      ;(println "Route number:" i)
      ;(println "New route:" (:tour new-x))
      ;(println "Customers to add:" cust)
      ;(println "---------------------------------------")
      (recur (assoc routes i new-x)
             (remove #(= % (last-customer new-x)) cust)
             (mod (inc i) K)
             t-matrix
             h-matrix))))

(defn- good-solution?
  "Compare the tour length of the incubent solution with the best solution so far"
  [s sbest]
  (let [max-length (* (apply max (mapv (comp count :tour) (:routes sbest))) ACO-PSI)]
    (every? #(< % max-length) (map (comp count :tour) (:routes s)))))

(defn ants-walk
  "Create a solution for each ant on the colony"
  [best-s i t-matrix t-matrix1 h-matrix]
  (if (== i ACO-NUM-ANTS)
    [best-s t-matrix1]
    (let [s (generate-solution (empty-solution) (range 1 N) 0 t-matrix h-matrix)
          best (if (and (feasible? s)
                        (< (:cost s) (:cost best-s))
                        (good-solution? s best-s))
                 s
                 best-s)
          t (release-pheromone t-matrix1 s)]
      ;(println "Ant:" i)
      ;(println "Sol:" (map :tour (:routes s)) "Cost:" (:cost s) "Feasible:" (feasible? s))
      ;(println "Best:" (map :tour (:routes best-s)) "Cost:" (:cost best-s))
      (recur best (inc i) t-matrix t h-matrix))))

(defn ant-colony
  "Perform Ant Colony Optimization to construct a new solution"
  [best-s i t-matrix h-matrix]
  ;(println "-----------------------------------------------")
  ;(println "Iter:" i)
  (if (== i ACO-MAX-ITER)
    best-s
    (let [t-matrix1 (dge t-matrix) ; Copy the pheromone matrix
          [best t] (ants-walk best-s 0 t-matrix t-matrix1 h-matrix)
          new-t (evaporate t)]
      (recur best (inc i) new-t h-matrix))))

;; Large Neighborhood Search
(defn start
  "Perfoms a Large Neighborhood Search on the solution s"
  [sbest i h]
  (if (or (== i LNS-MAX-ITER)
          (== (:cost sbest) OPTIMAL))
    sbest
    (let [s1 (copy-solution sbest)
          [s2 rb] (worst-removal (:routes s1))
          s3 (regret-2-insertion rb (:routes s2))
          new-best (if (and (feasible? s3) (< (:cost s3) (:cost sbest)))
                     s3
                     (let [t (build-pheromone-matrix (:cost sbest))
                           s4 (ant-colony sbest 0 t h)]
                       (if (< (:cost s4) (:cost sbest))
                         s4
                         sbest)))]
      (println "Iteration" i "Best" (map :tour (:routes sbest)) "Cost" (:cost sbest))
      (recur new-best (inc i) h))))

(defn lns
  "Search for an optimal solution using Hybrid Large Neighborhood Search"
  []
  ;(set-instance instance)
  ;(set-parameters params)
  (let [fmt-file (java.text.SimpleDateFormat. "ddMMyy-HHmmss")
        fmt-date (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm:ss")
        date-start (java.util.Date.)
        init (System/nanoTime)
        s (generate-random-solution)
        h (build-heuristic-matrix)
        sol (start s 0 h)
        end (System/nanoTime)
        date-end (java.util.Date.)
        file-output-name (str "./out/" (:name INSTANCE) "-" (.format fmt-file date-start) ".out")]
    (io/make-parents file-output-name)
    (spit file-output-name
          (json/generate-string {:instance (:name INSTANCE)
                                 :parameters (assoc PARAMS :wr-q WR-Q)
                                 :start (.format fmt-date date-start)
                                 :end (.format fmt-date date-end)
                                 :initial-solution {:routes (map :tour (:routes s)) 
                                                    :cost (:cost s)}
                                 :solution {:routes (map :tour (:routes sol))
                                            :cost (:cost sol)}
                                 :elapsed-time (/ (- end init) 1e9)
                                 :evaluations @NUM-EVALUATIONS
                                 :optimal OPTIMAL
                                 :dev (* (/ (- (:cost sol) OPTIMAL) OPTIMAL) 100)}
                                {:pretty true
                                 :key-fn #(string/replace (name %) #"-" "_")}))))

