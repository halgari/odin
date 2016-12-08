(defproject com.tbaldridge/odin "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha13"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/data.generators "0.1.2"]]
  ;:jvm-opts ["-agentpath:/Users/tim/lib/libyjpagent.jnilib"]
  :profiles {:dev {:dependencies [[org.clojure/data.xml "0.0.8"]
                                  [com.datomic/datomic-free "0.9.5407"]]}}
  :plugins [[autodoc/lein-autodoc "1.1.1"]
            [lein-doo "0.1.7"]
            [lein-cljsbuild "1.1.4"]]

  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:output-to "resources/public/js/testable.js"
                        :optimizations :simple}}]})
