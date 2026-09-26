#!/usr/bin/env -S clojure -M

(require '[futon3c.dispatch-with-recall :as dispatch])

(apply dispatch/-main *command-line-args*)
