(ns futon3c.wm.machinery-execution-cohort
  "Server-owned cohort binding for ordinary on-demand WM machinery clicks.

  HTTP request data cannot select this file, its digest, or its paths.  Once
  this namespace is reviewed and loaded, a missing, drifted, or inactive
  binding refuses before the Futon2 runner is invoked.  RUN4 already supplies
  its own independently checked `:execution-cohort` and is left unchanged."
  (:require [clojure.edn :as edn]
            [futon2.aif.c-fold-config :as digest]))

(def binding-path
  "/home/joe/code/futon3c/holes/labs/wm-contract/cohort-execution-binding.edn")

(def binding-sha256
  "8aafc58755d1e81a2d44a0bd84f0651e14632bd377268cebd56a1a57d0eaa1e9")

(def ^:dynamic *read-binding-text* #(slurp binding-path))

(defn- refuse! [reason]
  (throw (ex-info "Server-owned machinery cohort refused"
                  {:error :machinery-execution-cohort-refused
                   :reason reason})))

(defn- one-form [text]
  (try
    (with-open [reader (java.io.PushbackReader.
                        (java.io.StringReader. text))]
      (let [value (edn/read {:eof ::empty} reader)]
        (when (or (= ::empty value)
                  (not= ::end (edn/read {:eof ::end} reader)))
          (refuse! :binding-invalid))
        value))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse! :binding-unavailable-or-invalid))))

(defn execution-cohort
  "Return the exact active server binding, or refuse."
  []
  (let [text (try (*read-binding-text*)
                  (catch Throwable _ (refuse! :binding-unavailable)))
        _ (when-not (= binding-sha256 (digest/sha256 text))
            (refuse! :binding-sha256-mismatch))
        record (one-form text)
        cohort (:execution-cohort record)]
    (when-not (and (= #{:schema :status :execution-cohort} (set (keys record)))
                   (= :wm/server-owned-execution-cohort-v1 (:schema record))
                   (= :active (:status record))
                   (map? cohort)
                   (= #{:preregistration :data-root :cohort-id :sha256}
                      (set (keys cohort))))
      (refuse! :binding-invalid-or-inactive))
    cohort))

(defn apply-binding
  "Add the server-owned cohort unless an independently prepared server path
  (currently RUN4) already supplied one."
  [opts]
  (if (contains? opts :execution-cohort)
    opts
    (assoc opts :execution-cohort (execution-cohort))))
