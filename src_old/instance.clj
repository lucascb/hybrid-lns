(ns hybrid-lns.instance
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]))

;; Instance constants

(def ^:dynamic *instance*)
(def ^:dynamic *dimension*)
(def ^:dynamic *distances*)
(def ^:dynamic *capacity*)
(def ^:dynamic *demands*)
(def ^:dynamic *optimal*)

;; Algorithm parameters
(def ^:dynamic *seed*)
(def ^:dynamic *lns-max-iter*)
  
;; Worst removal parameters
(def ^:dynamic *wr-p*)

;; ACO parameters
(def ^:dynamic *aco-r1*)
(def ^:dynamic *aco-r2*)
(def ^:dynamic *aco-r3*)
(def ^:dynamic *aco-r1-r2*)
(def ^:dynamic *aco-alpha*)
(def ^:dynamic *aco-beta*)
(def ^:dynamic *aco-psi*)
(def ^:dynamic *aco-p*) ; Evaporation coefficient
(def ^:dynamic *aco-max-iter*)
(def ^:dynamic *aco-num-ants*)

(def ^:dynamic *max-runtime*)
(def ^:dynamic *date-start*)
(def ^:dynamic *start*)
(def ^:dynamic *evaluations*)

(def file-dateformat (java.text.SimpleDateFormat. "ddMMyy-HHmmss"))
(def result-dateformat (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm:ss"))

(defn termination-criteria-reached?
  "Stop if runtime exceeded max-runtime or solution reached optimum"
  [best-so-far]
  (or (= (:cost best-so-far) *optimal*)
      (>= (/ (- (System/nanoTime) *start*) 1e9) *max-runtime*)))

(defn- to-keyword
  ""
  [string]
  (keyword (clojure.string/replace string "_" "-")))

(defn read-file
  "Read and parse the specified JSON file"
  [filename]
  (json/parse-string (slurp filename) to-keyword))

(defn read-instances
  ""
  [path]
  (let [files (file-seq (io/file path))
        instances (filter #(.endsWith (str %) ".json") files)]
    (map read-file instances)))

(defn calculate-dev
  ""
  [best]
  (if (zero? *optimal*)
    {}
    {:optimal *optimal*
     :dev (* (/ (- (:cost best) *optimal*) *optimal*) 100)}))

(defn generate-result
  ""
  [best elapsed-time date-end]
  {:instance *instance*
   :parameters {:num-ants *num-ants*
                :alpha *alpha*
                :beta *beta*
                :evaporation-rate *evaporation-rate*
                :pertubation-ratio *pertubation-ratio*
                :num-elite-ants *num-elite-ants*
                :initial-trail *initial-trail*
                :initial-temperature *initial-temperature*
                :sa-max-tries *sa-max-tries*
                :lambda *lambda*
                :tabu-size *tabu-size*
                :max-iter *max-iter*
                :max-best-unchanged *max-best-unchanged*
                :max-best-so-far-not-improved *max-best-so-far-not-improved*
                :max-runtime *max-runtime*}
   :solution best
   :start (.format result-dateformat *date-start*)
   :end (.format result-dateformat date-end)
   :evaluations @*evaluations*
   :elapsed-time elapsed-time})

(defn write-result!
  ""
  [best-found]
  (let [end (System/nanoTime)
        date-end (java.util.Date.)
        start-date (.format file-dateformat *date-start*)
        fileoutput-name (str "./out/" *instance* "-" start-date ".out")
        elapsed-time (/ (- end *start*) 1e9)]
    ;;(log/info "Finished instance" *instance* "in" elapsed-time "seconds")
    (spit fileoutput-name
          (json/generate-string (merge (generate-result best-found
                                                        elapsed-time
                                                        date-end)
                                       (calculate-dev best-found))
                                {:pretty true}))))

