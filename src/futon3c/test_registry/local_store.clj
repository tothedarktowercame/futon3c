(ns futon3c.test-registry.local-store
  "Single construction point for the local test-registry backend."
  (:require [futon3c.test-registry.sqlite-backend :as sqlite]))

(defn path
  ([] (path nil))
  ([config]
   (or (:registry-db config)
       (System/getenv "REGISTRY_DB")
       sqlite/default-path)))

(defn open
  "Open the configured local registry or throw a typed failure. Tests may
  supply an already constructed :test-registry-backend."
  ([] (open nil))
  ([config]
   (if-let [backend (:test-registry-backend config)]
     backend
     (let [path (path config)]
       (try
         (sqlite/sqlite-backend path)
         (catch Throwable throwable
           (throw (ex-info "Local test-registry store unavailable"
                           {:record/type :test-registry/refusal
                            :reason :local-store-unavailable
                            :path path}
                           throwable))))))))
