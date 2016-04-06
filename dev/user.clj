(ns user
  (:require [figwheel-sidecar.config :as config]
            [figwheel-sidecar.repl-api :as api]))


(println "== starting fig with base config " (config/fetch-config))

(api/start-figwheel!
  (assoc (config/fetch-config) :build-ids ["test"])
  ;{:build-ids ["test"]}
  )

;;(require 'cemerick.piggieback)
;;(require 'cljs.repl.browser)
#_(defn cljs-repl []
  (cemerick.piggieback/cljs-repl
    (cljs.repl.browser/repl-env)
    :watch "src"
    :verbose true
    :output-to "repl-out/test.js"
    :output-dir "repl-out"
    ))
