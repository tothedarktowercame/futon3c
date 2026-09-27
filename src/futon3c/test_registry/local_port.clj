(ns futon3c.test-registry.local-port
  "Composition root for Futon2's read-only test-registry port."
  (:require [futon2.aif.registry-port :as port]
            [futon3c.evidence.backend :as backend]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.local-store :as local-store]
            [futon3c.test-registry.sqlite-backend :as sqlite]))

(defn registry-path [] (local-store/path))

(defn- verified-entry [store entry]
  (when entry
    (registry/read-chain! store (:evidence/id entry))
    entry))

(defn implementation
  "Construct the complete port over PATH. Every returned record has passed
  the registry's canonical chain verification."
  [path]
  (let [store (sqlite/sqlite-backend path)]
    {:entry (fn [id]
              (verified-entry store (backend/-get store id)))
     :latest-namespace (fn [namespace]
                         (verified-entry store
                           (sqlite/latest-run-for-namespace store namespace)))
     :latest-command (fn [command]
                       (verified-entry store
                         (sqlite/latest-run-for-command store command)))
     :runs (fn [{:keys [author since]}]
             (mapv #(verified-entry store %)
                   (backend/-query store
                     (cond-> {:query/tags [:test-registry]}
                       author (assoc :query/author author)
                       since (assoc :query/since since)))))}))

(defn install!
  "Install the local registry implementation. Safe to call again after load."
  ([] (install! (registry-path)))
  ([path] (port/install! (implementation path))))

(install!)
