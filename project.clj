(defproject com.roomkey/annotate "1.0.1"
  :description "Type annotations and type checking."
  :url "https://github.com/roomkey/annotate"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]]

  :plugins [[lein-cljsbuild "1.1.2"]]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :profiles {:repl {:plugins [[cider/cider-nrepl "0.12.0-SNAPSHOT"]]
                    :dependencies [[com.cemerick/piggieback "0.2.1"]]}

             :dev  {:source-paths ["dev"]
                    :plugins      [[codox "0.8.11"]
                                   [lein-gorilla "0.3.4"]
                                   [cider/cider-nrepl "0.12.0-SNAPSHOT"]]
                    :codox        {:src-dir-uri               "https://github.com/roomkey/annotate/blob/master/"
                                   :src-linenum-anchor-prefix "L"
                                   :output-dir                "."
                                   :exclude                   [annotate.examples]}}
             ;;:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             }

  :hooks [leiningen.cljsbuild]

  :cljsbuild {:test-commands {"test" ["node" "build/test/test.js"]}
              :builds {:main {:source-paths ["src" ;"test" ;"dev"
                                             ]}
                       :test {:source-paths ["src" "test"]
                              :compiler     {:optimizations  :simple
                                             :output-to      "build/test/test.js"
                                             :main           annotate.js.test-runner
                                             :cache-analysis true
                                             :parallel-build true
                                             :pretty-print   true}}}})
