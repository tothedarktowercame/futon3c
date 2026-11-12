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
;; Since the 2026-11-12 switch the default IS :7073; the property/env now
;; carries the rollback to :7093.
(def default live)

(deftest unset-env-resolves-to-the-live-store
  (testing "since the 2026-11-12 switch, unset means the live futon1b :7073"
    (is (= default (dog/resolve-minus40-base {})))
    (is (= default (lum/resolve-base {})))
    (is (= default (@#'http/resolve-scope-edge-base {})))
    ;; The vars themselves, resolved against this process's real env (which
    ;; does not carry FUTON_MEMORY_BASE).
    (is (= default dog/minus40-base))
    (is (= default (:base lum/defaults)))))

(deftest futon-memory-base-wins
  ;; the variable now carries the rollback to the retired scratch store
  (is (= legacy (dog/resolve-minus40-base {"FUTON_MEMORY_BASE" legacy})))
  (is (= legacy (lum/resolve-base {"FUTON_MEMORY_BASE" legacy})))
  (testing "scope edges honour FUTON3C_SCOPE_EDGE_BASE above FUTON_MEMORY_BASE"
    (is (= legacy (@#'http/resolve-scope-edge-base {"FUTON_MEMORY_BASE" legacy})))
    (is (= "http://example.invalid"
           (@#'http/resolve-scope-edge-base
             {"FUTON3C_SCOPE_EDGE_BASE" "http://example.invalid"
              "FUTON_MEMORY_BASE" legacy})))))

(deftest system-property-switches-a-running-jvm
  ;; A running JVM's environment cannot change, so a switch (or rollback) is
  ;; the JVM system property FUTON_MEMORY_BASE plus a reload.
  (let [reload! #(require 'futon3c.agency.dog-provider 'futon3c.xiang.lu-memory :reload)]
    (try
      (System/setProperty "FUTON_MEMORY_BASE" legacy)
      (reload!)
      (is (= legacy @(resolve 'futon3c.agency.dog-provider/minus40-base)))
      (is (= legacy (:base @(resolve 'futon3c.xiang.lu-memory/defaults))))
      (finally
        (System/clearProperty "FUTON_MEMORY_BASE")
        (reload!)))
    (is (= default @(resolve 'futon3c.agency.dog-provider/minus40-base))
        "clearing the property and reloading returns to the default")))
