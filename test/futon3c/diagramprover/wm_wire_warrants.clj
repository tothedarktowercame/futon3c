(ns futon3c.diagramprover.wm-wire-warrants
  "The warrant lookup the wire ledger uses. Kept out of
  futon3c.diagramprover.wm-wire so that a wire test, which loads that
  namespace for the first-layer check, does not load the test registry."
  (:require [futon3c.test-registry.local-port :as local-port]
            [futon3c.test-registry.sqlite-backend :as sqlite]))

(def ^:dynamic *warrant-store-path*
  "Local warrant database override. Nil selects REGISTRY_DB, then
  the test registry's canonical local path."
  nil)

(defn warrant-store-path []
  (or *warrant-store-path*
      (System/getenv "REGISTRY_DB")
      sqlite/default-path))

(defn latest-local-run
  "Ask the registry's one currentness operation for NAMESPACE in REPO.
  The operation may durably request, but never executes or waits for, a run."
  [namespace repo]
  ((:current-or-request (local-port/implementation (warrant-store-path)))
   {:namespace namespace :repo repo}))
