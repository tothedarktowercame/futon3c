(ns futon3c.agency.memory-base-test
  "FUTON_MEMORY_BASE precedence tests (Joe 2026-11-12, M-repl-circus MEM):
  every memory-store client in futon3c resolves its base URL from one env
  variable first, falling back to exactly its historical value. With the
  variable unset each must resolve to the pre-migration URL. Run:
  clojure -M:test -n futon3c.agency.memory-base-test"
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.dog-provider :as dog]
            [futon3c.xiang.lu-memory :as lum]
            [futon3c.transport.http :as http]))

(def live "http://127.0.0.1:7073")
(def legacy "http://127.0.0.1:7093")

(deftest unset-env-resolves-to-historical-default
  (testing "no behaviour change until FUTON_MEMORY_BASE is set"
    (is (= legacy (dog/resolve-minus40-base {})))
    (is (= legacy (lum/resolve-base {})))
    (is (= legacy (@#'http/resolve-scope-edge-base {})))
    ;; The vars themselves, resolved against this process's real env (which
    ;; does not carry FUTON_MEMORY_BASE): same URLs as before the change.
    (is (= legacy dog/minus40-base))
    (is (= legacy (:base lum/defaults)))))

(deftest futon-memory-base-wins
  (is (= live (dog/resolve-minus40-base {"FUTON_MEMORY_BASE" live})))
  (is (= live (lum/resolve-base {"FUTON_MEMORY_BASE" live})))
  (testing "scope edges honour FUTON3C_SCOPE_EDGE_BASE above FUTON_MEMORY_BASE"
    (is (= live (@#'http/resolve-scope-edge-base {"FUTON_MEMORY_BASE" live})))
    (is (= "http://example.invalid"
           (@#'http/resolve-scope-edge-base
             {"FUTON3C_SCOPE_EDGE_BASE" "http://example.invalid"
              "FUTON_MEMORY_BASE" live})))))

(deftest system-property-switches-a-running-jvm
  ;; A running JVM's environment cannot change, so the switch is the JVM
  ;; system property FUTON_MEMORY_BASE plus a reload of the three namespaces.
  (let [reload! #(require 'futon3c.agency.dog-provider 'futon3c.xiang.lu-memory :reload)]
    (try
      (System/setProperty "FUTON_MEMORY_BASE" live)
      (reload!)
      (is (= live @(resolve 'futon3c.agency.dog-provider/minus40-base)))
      (is (= live (:base @(resolve 'futon3c.xiang.lu-memory/defaults))))
      (finally
        (System/clearProperty "FUTON_MEMORY_BASE")
        (reload!)))
    (is (= legacy @(resolve 'futon3c.agency.dog-provider/minus40-base))
        "clearing the property and reloading rolls back")))
