(defproject parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [uncomplicate/neanderthal "0.11.0"]
                 [random-seed "1.0.0"]
                 [cheshire "5.8.0"]]
  :main ^:skip-aot hybrid-lns.core
  :target-path "target/%s"
  :jvm-opts ["-Xmx1G"]
  :profiles {:uberjar {:aot :all}})
