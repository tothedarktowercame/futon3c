(ns futon3c.apm.frame-specification
  "Fail-closed ingestion for Ground Control's declarative frame specification."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-machine :as machine]))

(def roles #{:solver :student :guide :scribe :proctor :analyst})

(def required-keys
  #{:frame/id :frame/specification-version :frame/mode
    :frame/control-branch :frame/control-base :problem/id
    :problem/classification :problem/repository :problem/branch
    :problem/revision :problem/path :problem/blob :problem/preflight
    :frame/seat-policy :frame/timeout-policy :frame/cast
    :frame/continuation-policy :frame/separation-policy
    :countdown/block :qualification/plan})

(defn validate
  "Validate the authored data shape. This does not claim that runtime evidence
  (cast readiness, wake behavior, checkout cleanliness) has been observed."
  [spec expected-frame-id expected-digest]
  (let [missing (->> required-keys (remove #(contains? spec %)) set)
        seat-roles (set (keys (:frame/seat-policy spec)))
        cast-roles (set (keys (:frame/cast spec)))
        errors (cond-> []
                 (not (map? spec)) (conj :specification-not-map)
                 (seq missing) (conj :required-keys-missing)
                 (not= 1 (:frame/specification-version spec))
                 (conj :specification-version-unsupported)
                 (not= roles seat-roles) (conj :seat-policy-roles-incomplete)
                 (not= roles cast-roles) (conj :cast-roles-incomplete)
                 (not= expected-frame-id (:frame/id spec))
                 (conj :frame-id-mismatch)
                 (and expected-digest
                      (not= expected-digest (machine/ledger-digest [spec])))
                 (conj :registration-digest-mismatch))]
    {:valid? (empty? errors)
     :frame-matches? (= expected-frame-id (:frame/id spec))
     :registration-matches? (or (nil? expected-digest)
                                (= expected-digest
                                   (machine/ledger-digest [spec])))
     :digest (when (map? spec) (machine/ledger-digest [spec]))
     :errors errors :missing-keys missing
     :specification spec}))

(defn ingest [path expected-frame-id expected-digest]
  (try
    (validate (edn/read-string (slurp path)) expected-frame-id expected-digest)
    (catch Throwable t
      {:valid? false :frame-matches? false :registration-matches? false
       :digest nil
       :errors [:specification-unreadable]
       :finding {:message (.getMessage t)}})))
