(ns hybrid-lns.solution)
  (:use [hybrid-lns.instance])

(defstruct Solution :tour :cost)

(defn split-tour
  "Split the tour into routes"
  [tour]
  (->> tour
       (partition-by zero?)
       (remove #(= % '(0)))
       (map vec)))

;; Generate solution using Savings Heuristic
(defn- merge-routes
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

(defn- enough-capacity?
  "Check whether the merge violates the capacity constraint of the vehicle"
  [a b]
  (<= (+ (:load a) (:load b)) Q))

(defn- customer-route
  "Return the route which serves the customer i"
  [routes i]
  (let [r (first routes)
        t (:tour r)]
    (if (some #(= % i) t)
      r
      (recur (rst routes) i))))

(defn- remove-routes
  "Remove the routes r1 and r2 from the list of routes"
  [routes r1 r2]
  (remove #(= % r2)
          (remove #(= % r1) routes)))

(defn- calculate-savings
  "Calculate the Savings value between each pair of nodes"
  []
  (flatten
   (for [i (range 1 N)]
    (for [j (range 1 N) :when (< i j)]
      {:from i :to j :saving (- (+ (entry *distances* i 0)
                                   (entry *distances* 0 j))
                                (entry *distances* i j))}))))

(defn- generate-solution
  "Merge each two routes from the savings list"
  [savings routes]
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

(defn build-savings-solution
  "Generate a feasible solution using Savings Heuristic"
  []
  (let [savings (sort-by :saving > (calculate-savings))
        routes (for [i (range 1 *dimension*)]
                 {:tour [i] :load (nth *demands* i)})]
    ;(println "****** SAVINGS ******")
    ;(println savings)
    (build-solution
     (map build-route (map :tour (generate-solution savings routes))))))


