(ns parser.core
  (:require [clojure.string :as s])
  (:gen-class))

;; Utils
(defn str->int
  "Convert a string to an integer"
  [str]
  (Integer. str))

(defn str->keyword
  "Convert a string to a keyword"
  [str]
  (keyword (s/lower-case (s/replace str "_" "-"))))

(defn slice
  "Equivalent to l[s:e]"
  [l s e]
  (drop (inc s) (take (inc e) l)))

(defn at
  "Equivalent to m[i][j]"
  [m i j]
  (nth (nth m i) j))

;; Data types and type conversions
(defrecord Section [x y d])

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

(defn calc-dist
  "Calculate the distance between each section"
  [sections]
  (let [n (count sections)]
    (for [s1 (range 1 (inc n))]
      (for [s2 (range 1 (inc n))]
        (euc-distance ((keyword (str s1)) sections)
                      ((keyword (str s2)) sections))))))

;; Parsing functions
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

(defn new-section
  "Create a new section of form #{:n Section{x:, y:, d:}}"
  [[coord x y] [_ d]]
  (hash-map (keyword (str coord)) (Section. (str->int x) 
                                            (str->int y) 
                                            (str->int d))))

(defn parse-section
  "Parse a line to a new section"
  [coord demand]
  (new-section (s/split (s/trim coord) #" ") 
               (s/split (s/trim demand) #" ")))

(defn parse-sections
  "Parse each line and transform to a hash-map of Coords"
  [coords demands]
  (reduce into {} (map parse-section coords demands)))

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

(defn parse-body
  "Parse body values based on header"
  [header lines]
  (let [dim (:dimension header)
        coords (.indexOf lines "NODE_COORD_SECTION ")
        demands (.indexOf lines "DEMAND_SECTION ")
        depot (.indexOf lines "DEPOT_SECTION ")]
    (hash-map :sections 
              (parse-sections (slice lines coords (+ coords dim))
                              (slice lines demands (+ demands dim)))
              :depot
              (str->keyword (s/trim (nth lines (inc depot)))))))

(defn parse-file
  "Parse the file and create a hash-map"
  [filename]
  (let [file (slurp filename)
        header (parse-header file)
        body (parse-body header (s/split-lines file))
        dist (calc-dist (:sections body))]
    (into header (into body {:distances dist}))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
