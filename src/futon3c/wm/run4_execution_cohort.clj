(ns futon3c.wm.run4-execution-cohort
  "Strict server-side validation for a pinned RUN4 execution cohort.

  Capacity remains authoritative at Futon2's race-safe start boundary. This
  preflight prevents knowingly exhausted or drifted cohorts from consuming a
  RUN4 admission/click before that boundary."
  (:require [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]))

(def cohort-keys #{:preregistration :data-root :cohort-id :sha256})

(defn- refuse! [reason]
  (throw (ex-info "RUN4 execution cohort refused"
                  {:error :run4-execution-cohort-refused :reason reason})))

(defn validate-and-preflight!
  "Validate exact server-owned COHORT and call PREFLIGHT on its captured bytes.

  PREFLIGHT is Futon2's read-only `execution-preflight` port. This boundary
  checks only its public summary; the internal captured `:snapshot` is neither
  retained nor serialized here.
  The preregistration file is reread after the port call to close preflight
  drift. Race-safe capacity is still rechecked by the runner at start."
  [cohort preflight]
  (when-not (and (map? cohort) (= cohort-keys (set (keys cohort)))
                 (string? (:preregistration cohort))
                 (.isAbsolute (io/file (:preregistration cohort)))
                 (string? (:data-root cohort))
                 (.isAbsolute (io/file (:data-root cohort)))
                 (keyword? (:cohort-id cohort))
                 (not (namespace (:cohort-id cohort)))
                 (string? (:sha256 cohort))
                 (re-matches #"[0-9a-f]{64}" (:sha256 cohort))
                 (fn? preflight))
    (refuse! :invalid-cohort-contract))
  (let [prereg (.getCanonicalFile (io/file (:preregistration cohort)))
        root (.getCanonicalFile (io/file (:data-root cohort)))]
    (when-not (and (= (.getAbsolutePath prereg) (:preregistration cohort))
                   (= (.getAbsolutePath root) (:data-root cohort))
                   (.isFile prereg) (.isDirectory root))
      (refuse! :cohort-authority-invalid))
    (let [bytes (slurp prereg)]
      (when-not (= (:sha256 cohort) (digest/sha256 bytes))
        (refuse! :cohort-preregistration-drift))
      (let [result (preflight cohort)
            summary (select-keys result [:cohort-id :target :remaining])]
        (when-not (and (map? result)
                       (= #{:cohort-id :target :remaining} (set (keys summary)))
                       (= (:cohort-id cohort) (:cohort-id summary))
                       (pos-int? (:target summary))
                       (pos-int? (:remaining summary))
                       (<= (:remaining summary) (:target summary)))
          (refuse! :cohort-unavailable-or-exhausted))
        (when-not (= bytes (slurp prereg))
          (refuse! :cohort-preregistration-drift))
        cohort))))
