(ns hybrid-lns.core
  (:require [hybrid-lns.hybrid-lns :as lns]
            [clojure.java.io :as io]
            [cheshire.core :as json])
  (:gen-class))

(defn- to-keyword
  ""
  [string]
  (keyword (clojure.string/replace string "_" "-")))

(defn create-initial-solution
  "Defines an initial solution based using ACO"
  []
  (let [h (lns/build-heuristic-matrix)
        t (lns/build-pheromone-matrix 1000.0)]
    (lns/ant-colony {:routes [] :cost Integer/MAX_VALUE} 0 t h)))

(defn read-files
  "Read all output files and return its data"
  [files]
  (map #(json/parse-string (slurp %) to-keyword) files))

(defn run-benchmark
  "Run the algorithm on each instance files"
  [paramfile file-dir]
  (let [files (file-seq (io/file file-dir))
        instances (read-files (filter #(.endsWith (str %) ".json") files))
        params (json/parse-string (slurp paramfile) to-keyword)]
    (println "------------- USING PARAMETERS FROM" paramfile " ----------------")
    (lns/set-parameters params)
    (doseq [instance instances]
      (println "----------- INSTANCE" (:name instance) "-----------")
      (lns/set-instance instance)
      (lns/lns))))

(defn -main
  "Perfom the algorithm on the instance file or on each instance files of the specified folder"
  [& args]
  (println args)
  (if (= (count args) 2)
    (let [params (first args)
          files (second args)]
      (run-benchmark params files))
    (println "Wrong number of arguments")))
