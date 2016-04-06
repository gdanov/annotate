(defproject com.roomkey/annotate "1.0.1"
  :description "Type annotations and type checking."
  :url "https://github.com/roomkey/annotate"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.40"]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-figwheel "0.5.0-5"]
            ]

  :figwheel {:reload-clj-files {:clj true :cljc true}}

  :profiles {:repl {:source-paths ["dev"]
                    :plugins      [[cider/cider-nrepl "0.12.0-SNAPSHOT"]]
                    :dependencies [[com.cemerick/piggieback "0.2.1"]
                                   [figwheel-sidecar "0.5.0-5"]]
                    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}

             :dev  {:plugins      [[codox "0.8.11"]
                                   [lein-gorilla "0.3.4"]
                                   [cider/cider-nrepl "0.12.0-SNAPSHOT"]]
                    :codox        {:src-dir-uri               "https://github.com/roomkey/annotate/blob/master/"
                                   :src-linenum-anchor-prefix "L"
                                   :output-dir                "."
                                   :exclude                   [annotate.examples]}}
             ;;:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             }

  ;;:hooks [leiningen.cljsbuild]

  :cljsbuild {:test-commands {"test" ["node" "build/test/test.js"]}
              :builds        {:main {:source-paths ["src" ;"test" ;"dev"
                                                    ]}
                              :test {:source-paths ["src" "test"]
                                     :figwheel     {:load-warninged-code true}
                                     :compiler     {:optimizations  :none
                                                    :output-to      "resources/public/test.js"
                                                    :output-dir     "resources/public/out"
                                                    :asset-path "out"
                                                    :main           annotate.js.test-runner
                                                    :cache-analysis true
                                                    ;;:parallel-build true
                                                    :pretty-print   true}}}})
