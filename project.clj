(defproject aoc_2015 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [digest "1.4.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.json "0.2.6"]]
  :main ^:skip-aot aoc-2015.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
