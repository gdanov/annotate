(enable-console-print!)

(ns annotate.js.test-runner
  (:require [cljs.test]
            [annotate.util-test]
            [annotate.types-test]
            [annotate.fns-test]))

(println "===== running tests in Script mode ====")

(cljs.test/run-tests 'annotate.util-test 'annotate.types-test 'annotate.fns-test)
