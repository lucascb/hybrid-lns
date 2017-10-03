(ns lns-aco
  (:require [parser :as p])
  (:use [uncomplicate.neanderthal core native]))

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

(defn empty-route
  "Returns an empty route"
  [d]
  (let [n (ncols d)]
    {:matrix (dge n n) :cost 0 :tour []}))

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
(defn remove-customer
  "Removes the customer i from the route x"
  [x i d]
  (let [nx (remove #(= % i) (:tour x))]
    (build-route nx d)))

(defn routes-without-customers
  "Generates routes without each customer and calculates its cost"
  [s d]
  (flatten (for [x s]
             (for [i (:tour x)]
               (let [r (remove-customer x i d)]
                 {:customer i
                  :delta-cost (- (:cost x) (:cost r))
                  :route r
                  :old-tour (:tour x)})))))

(defn update-solution
  "Update the solution s to remove the customer chosen in worst-removal"
  [s rc]
  (conj (remove #(= (:tour %) (:old-tour rc)) s) (:route rc)))

(defn worst-removal
  "Removes randomly q worst costumers in the solution s"
  [s rb q d p]
  (if (= q 0) ; Returns the new solution (s) alongside with the
    [s rb]    ; customers removed in the removed bank (rb)
    (let [l (sort-by :delta-cost > (routes-without-customers s d))
          y (rand)
          rc (nth l (int (* (Math/pow y p) (count l))))] ; Chosen customer to remove
      (recur (update-solution s rc)
             (conj rb (:customer rc))
             (dec q)
             d
             p))))

;; Insertion heuristic
(defn can-add?
  "Checks if the customer i can be added to the route x
  given q: max capacity of a vehicle and c: demands of customers"
  [i x q c]
  (let [i-demand (nth c i)]
    (<= (+ i-demand (:cost x)) q)))

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
    (doseq [i (range n)]
      (doseq [j (range n)]
        (if (= i j)
          (entry! s i j 0)
          (entry! s i j (+ (entry d i 0)
                           (- (entry d 0 j)
                              (entry d i j)))))))
    s))

(defn build-pheromone-matrix
  "Build the pheromone trail matrix"
  [size]
  (let [t (dge size size)]
    (doseq [i (range size)]
      (doseq [j (range size)]
        (entry! t i j 1.0)))
    t))

(defn exploitation
  "Defines the next customer based on exploitation, given a: available customers, i: last customer added, t: the pheromone trail matrix, n: heuristic matrix, alpha and beta algorithm parameters"
  [a i t n alpha beta]
  (let [exps (for [j a] {:cust j :val (* (Math/pow (entry t i j) alpha)
                                         (Math/pow (entry n i j) beta))})]
    (println "Performing exploitation")
    (:cust (apply max-key :val exps))))

(defn build-probabilities
  "Builds the probabilities of each possible customer to be inserted"
  [a i t n alpha beta]
  (let [x (reduce + (for [j a] (* (Math/pow (entry t i j) alpha)
                                  (Math/pow (entry n i j) beta))))
        probs (for [j a] {:cust j :prob (/ (* (Math/pow (entry t i j) alpha)
                                              (Math/pow (entry n i j) beta))
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
  [a i t n alpha beta]
  (let [probs (build-probabilities a i t n alpha beta)
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
    (let [i (last-customer x) ; Last customer added to the route
          r (rand)
          j (cond (<= r r1)
                  (exploitation a i t n alpha beta)
                  (and (< r1 r) (<= r (+ r1 r2)))
                  (biased-exploration a i t n alpha beta)
                  (and (< (+ r1 r2) r) (<= r (+ r1 r2 r3)))
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

(def size (:dimension vrp))

(def h (build-heuristic-matrix d))
(def t (build-pheromone-matrix size))
(def x (empty-route d))

(def r1 0.5)
(def r2 0.3)
(def r3 0.2)

(def sol [[21 31 19 17 13 7 26] [12 1 16 30] [27 24] [29 18 8 9 22 15 10 25 5 20] [14 28 11 4 23 3 6]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
