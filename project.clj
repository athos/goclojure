(defproject goclojure "0.1.0-SNAPSHOT"
  :description "Brings goruby like abbreviation to Clojure"
  :url "https://github.com/athos/goclojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.6.1"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.854"]
                 [org.clojure/math.combinatorics "0.1.3"]]
  :profiles {:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :dev {:source-paths ["examples"]}}
  :aliases {"all" ["with-profile" "dev:1.7:1.8"]})
