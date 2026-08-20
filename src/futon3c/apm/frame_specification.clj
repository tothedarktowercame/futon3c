(ns futon3c.apm.frame-specification
  "Fail-closed ingestion for Ground Control's declarative frame specification."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]))

(def roles #{:solver :student :guide :scribe :proctor :analyst})

(def required-keys
  #{:frame/id :frame/specification-version :frame/mode
    :frame/control-branch :frame/control-base :problem/id
    :problem/classification :problem/repository :problem/branch
    :problem/revision :problem/path :problem/blob :problem/preflight
    :frame/seat-policy :frame/timeout-policy :frame/cast
    :frame/apparatus
    :frame/continuation-policy :frame/separation-policy
    :countdown/block :qualification/plan})

(defn- sha40? [value]
  (boolean (and (string? value) (re-matches #"[0-9a-f]{40}" value))))

(defn- pinned-blob [repository revision path]
  (let [{:keys [exit out]} (shell/sh "git" "-C" repository "rev-parse"
                                     (str revision ":" path))]
    (when (zero? exit) (str/trim out))))

(defn- apparatus-errors [spec]
  (let [{:keys [repository revision role-cards]} (:frame/apparatus spec)]
    (cond
      (not (and (string? repository) (not (str/blank? repository))
                (sha40? revision) (map? role-cards)
                (= roles (set (keys role-cards)))))
      [:apparatus-shape-invalid]

      (not (every? (fn [[_ {:keys [path blob]}]]
                     (and (string? path) (not (str/blank? path))
                          (sha40? blob)))
                   role-cards))
      [:apparatus-role-card-pin-invalid]

      :else
      (cond-> []
        (not-every? (fn [[_ {:keys [path blob]}]]
                      (= blob (pinned-blob repository revision path)))
                    role-cards)
        (conj :apparatus-revision-mismatch)))))

(defn- policy-errors [spec]
  (let [seats (:frame/seat-policy spec)
        frame-minutes (get-in spec [:frame/timeout-policy :frame-minutes])]
    (cond-> []
      (not (every? #(and (integer? (get-in seats [% :turn-minutes]))
                          (<= 60 (get-in seats [% :turn-minutes]))) roles))
      (conj :seat-turn-policy-insufficient)
      (not (<= 5 (or (get-in seats [:student :request-minutes]) -1)))
      (conj :student-request-policy-insufficient)
      (not (and (integer? frame-minutes) (<= 240 frame-minutes)))
      (conj :frame-timeout-policy-insufficient)
      (not= {:durable-required? true :wake-test-required? true}
            (:frame/continuation-policy spec))
      (conj :continuation-policy-incomplete)
      (not= {:author-reviewer-distinct? true :arms-isolated? true}
            (:frame/separation-policy spec))
      (conj :separation-policy-incomplete))))

(defn validate
  "Validate the authored data shape. This does not claim that runtime evidence
  (cast readiness, wake behavior, checkout cleanliness) has been observed."
  [spec expected-frame-id expected-digest]
  (let [missing (->> required-keys (remove #(contains? spec %)) set)
        seat-roles (set (keys (:frame/seat-policy spec)))
        cast-roles (set (keys (:frame/cast spec)))
        errors (into (concat (policy-errors spec) (apparatus-errors spec))
                     (cond-> []
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
                 (conj :registration-digest-mismatch)))]
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
