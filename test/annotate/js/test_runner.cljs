(enable-console-print!)

(ns annotate.js.test-runner
  (:require [cljs.nodejs]
            [cljs.test]
            [annotate.util-test]))

(println "===== running tests in Script mode ====")

(cljs.test/run-tests 'annotate.util-test)
