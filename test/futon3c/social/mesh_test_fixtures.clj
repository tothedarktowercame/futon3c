(ns futon3c.social.mesh-test-fixtures
  "Explicit volatile mesh and clock storage for unit tests, including executors."
  (:require [futon3c.evidence.store :as estore]
            [futon3c.agency.clock-decision :as decision]
            [futon3c.social.coordination-ledger :as ledger]))

(defn with-store [f]
  ;; These namespaces execute tests sequentially. Root rebinding also covers
  ;; raw executor Runnables, which do not convey Clojure dynamic bindings.
  (with-redefs [ledger/*test-evidence-store* estore/!store
                decision/*test-store* estore/!store
                decision/*repo-roots* {}]
    (f)))
