(ns parser
  (:require [clojure.string :as s])
  (:use [uncomplicate.neanderthal core native]))

;; Neanderthal conversions
(defn to-neanderthal-matrix
  "Converts a Clojure list of lists to a neanderthal matrix"
  [matrix]
  (let [n (count matrix)
        values (flatten matrix)]
    (dge n n values {:order :row})))

;; Utils
(defn str->int
  "Convert a string to an integer"
  [str]
  (Integer. str))

(defn str->keyword
  "Convert a string to a keyword"
  [str]
  (keyword (s/lower-case (s/replace str "_" "-"))))

(defn at
  "Equivalent to m[i][j]"
  [m i j]
  (nth (nth m i) j))

;; Data types and type conversions
(defrecord Section [x y])

(defn parse-comment
  "Parse the comment and divide into three keys"
  [comment]
  (let [fields (map s/trim
                    (s/split (subs comment 1 (dec (count comment)))
                             #","))
        author (first fields)
        trucks (str->int (last (s/split (second fields) #" ")))
        optimal (str->int (last (s/split (last fields) #" ")))]
    (hash-map :author author
              :no-of-trucks trucks
              :optimal-value optimal)))

(def conversions {:name identity
                  :comment parse-comment
                  :type identity
                  :dimension str->int
                  :edge-weight-type identity
                  :capacity str->int})

(defn convert
  "Convert a value based on its key"
  [k v]
  ((k conversions) v))

;; Distance functions
(defn euc-distance
  "Calculate the euclidean distance between two Sections"
  [s1 s2]
  (let [xd (- (:x s1) (:x s2))
        yd (- (:y s1) (:y s2))]
    (Math/round (Math/sqrt (+ (* xd xd) (* yd yd))))))

(defn calc-dist-matrix
  "Calculate the distance between each section"
  [sections]
  (let [n (count sections)]
    (for [s1 (range 1 (inc n))]
      (for [s2 (range 1 (inc n))]
        (euc-distance ((keyword (str s1)) sections)
                      ((keyword (str s2)) sections))))))

;;; Parsing functions

;; Section parsing
(defn new-section
  "Create a new section of form #{:n Section{x:, y:, d:}}"
  [[coord x y]]
  (hash-map (keyword (str coord)) (Section. (str->int x) 
                                            (str->int y))))

(defn parse-section
  "Parse a line to a new section"
  [coord]
  (new-section (s/split (s/trim coord) #" ")))

(defn parse-sections
  "Parse each line and transform to a hash-map of Coords"
  [coords]
  (reduce into (map parse-section coords)))

;; Demands parsing
(defn parse-demand
  "Parse a line to a new demand"
  [line]
  (let [[section demand] (s/split (s/trim line) #" ")]
       ;(hash-map (keyword section)(str->int demand))
    (str->int demand)))

(defn parse-demands
  "Parse each line and create a hash-map of demands"
  [lines]
  ;(reduce into (map parse-demand lines))
  (map parse-demand lines))

;; Matrix parsing
(defn parse-lines-of-matrix
  "Parse each line of the matrix"
  [lines]
  (map str->int (filter #(not (empty? %)) 
                        (s/split (reduce str lines) #" "))))

(defn create-inf-lines
  "Separate each inferior lines of the matrix"
  [lines n]
  (let [create-line
        (fn [line curr i n]
          (if (empty? curr)
            line
            (recur (conj line (take i curr))
                   (drop i curr)
                   (inc i)
                   n)))]
    (create-line [] lines 1 n)))

(defn create-weights-matrix
  "Parse a file and create a weights matrix of size n x n"
  [lines n]
  (let [values (parse-lines-of-matrix lines)
        inf-lines (create-inf-lines values n)]
    (for [i (range n)]
      (for [j (range n)]
        (cond (> i j) (at inf-lines (dec i) j)
              (< i j) (at inf-lines (dec j) i)
              :else 0)))))

;; Header parsing
(defn break
  "Break the string into a pair of key and value"
  [str]
  (s/split (s/trim str) #" : " 2))

(defn add-to-hashmap
  "Parse a line and add it to the hashmap"
  [[k v]]
  (let [key (str->keyword k)]
       (hash-map key
                 (convert key v))))

(defn parse-header
  "Parse header values from the file"
  [file]
  (reduce into {} (map add-to-hashmap
                       (map break (re-seq #"[A-Z_]+ : .*" file)))))

;; Body parsing
(defn parse-body-with-coords
  "Parse euc-2d body values based on header"
  [header lines]
  (let [dim (:dimension header)
        type (:edge-weight-type header)
        coords (.indexOf lines "NODE_COORD_SECTION ")
        demands (.indexOf lines "DEMAND_SECTION ")
        depot (.indexOf lines "DEPOT_SECTION ")
        sections (parse-sections (subvec lines
                                         (inc coords)
                                         (+ coords dim 1)))]
    (hash-map :sections
              sections
              :demands
              (parse-demands (subvec lines
                                     (inc demands)
                                     (+ demands dim 1)))
              :depot
              (str->keyword (s/trim (nth lines (inc depot))))
              :distances
              (calc-dist-matrix sections))))

(defn parse-body-with-matrix
  "Parse explicit distance values based on header"
  [header lines]
  (let [dim (:dimension header)
        w (.indexOf lines "EDGE_WEIGHT_SECTION")
        demands (.indexOf lines "DEMAND_SECTION")
        depot (.indexOf lines "DEPOT_SECTION")]
    (hash-map :distances
              (create-weights-matrix (subvec lines (inc w) demands)
                                     dim)
              :demands
              (parse-demands (subvec lines
                                     (inc demands)
                                     (+ demands dim 1)))
              :depot
              (str->keyword (s/trim (nth lines (inc depot)))))))

;; File parsing
(defn parse-file
  "Parse the file and create a hash-map"
  [filename]
  (let [file (slurp filename)
        header (parse-header file)
        type (:edge-weight-type header)]
    (cond (= type "EUC_2D")
          (into header
                (parse-body-with-coords header (s/split-lines file)))
          (= type "EXPLICIT")
          (into header
                (parse-body-with-matrix header (s/split-lines file)))
          :else header)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
