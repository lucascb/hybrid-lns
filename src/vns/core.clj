(ns teste.core
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
  (let [edges (map vector tour (concat (rest tour) '(1)))
        costs (for [e edges] (at dist-matrix (first e) (last e)))]
    (reduce + costs)))

(defn calculate-load
  "Calculates the total load of a tour"
  [tour]
  (reduce + (map demand tour))

(defn new-solution
  "Returns a solution given its attributes"
  [r n c]
  {:routes r :no-of-trucks n :cost c})

;; Generates the initial solution
(defn load-truck
  "Loads randomly a truck"
  [cust tour ld cap]
  (if (empty? cust) ; If the customers list is empty,
    (cons 1 tour)   ; return to the depot.
    (let [selected-cust (rand-nth cust) ; Choose a non-visited customer
          dem (demand selected-cust)]   ; randomly.
      (if (>= (+ ld dem) cap) ; If the selected customer demand doesnt fit,
        (cons 1 tour)         ; return to the depot.
        (recur (remove #(= % selected-cust) cust) ; Otherwise, add it to the
               (cons selected-cust tour)          ; vehicle route and repeat
               (+ ld dem)                         ; the function.
               cap)))))

(defn load-trucks
  "Loads randomly each vehicles to visit all the customers"
  [cust tours cap]
  (if (empty? cust) tours
      (let [tour (load-truck cust '(1) 0 cap)]
        (recur (remove #(in? % tour) cust)
               (cons tour tours)
               cap))))

(defn generate-initial-solution
  "Generates a randomly initial solution"
  [customers problem]
  (let [cap (:capacity problem)
        dist-matrix (:distances problem)
        routes (load-trucks customers '() cap)
        costs (for [r routes] (cost dist-matrix r))]
    (new-solution routes
                  (count routes)
                  (reduce + costs))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
  
