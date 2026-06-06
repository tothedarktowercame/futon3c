(ns futon3c.wm.guardrails
  "Guardrails for WM pilot autonomous action selection.

   This namespace is deliberately pure at the predicate boundary: callers can
   inject mission/path lookups in CTX, while the default mission lookup uses the
   hardened futon2 mission registry through requiring-resolve."
  (:require [clojure.string :as str]))

(def autonomous-action-types
  #{:address-sorry :fire-pattern :open-mission})

(def operator-only-action-types
  #{:learn-action-class})

(def outward-op-pattern
  #"(?i)\b(send|sends|sent|email|eoi|invoice|post|publish|deliver)\b")

(def guarded-marker-pattern
  #"(?i)(∇|nabla|deform|protocol-defining|protocol defining|niche-construction|niche construction)")

(defn- action-type [action]
  (some-> (:type action) keyword))

(defn- action-target [action]
  (or (:target action) (:path action) (:target-file action) (:mission-path action)))

(defn default-forbidden-path?
  "Conservative path guard for hard NO regions."
  [path]
  (let [s (str path)]
    (boolean
     (or (str/includes? s "/.state/")
         (str/includes? s "\\.state\\")
         (re-find #"(^|/)futon7b($|/)" s)
         (re-find #"(?i)futon7-private|futon7_PRIVATE|PRIVATE-leak" s)))))

(defn- target-forbidden?
  [action forbidden-path?]
  (boolean
   (some (fn [x]
           (and (some? x) (forbidden-path? x)))
         [(:target action) (:path action) (:target-file action)
          (:mission-path action) (:repo action)])))

(defn- marker-present?
  [action]
  (boolean
   (or (:delta-grad? action)
       (:nabla-deform? action)
       (:protocol-defining? action)
       (:niche-construction? action)
       (some (fn [x] (re-find guarded-marker-pattern (str x)))
             [(:marker action) (:markers action) (:rationale action)
              (:why action) (:title action)]))))

(defn- outward-op?
  [action]
  (boolean
   (some (fn [x] (re-find outward-op-pattern (str x)))
         [(action-type action) (:target action) (:rationale action)
          (:title action) (:unblock-action action)])))

(defn- positive-open-hole-count?
  [mission-status]
  (let [n (or (:open-hole-count mission-status)
              (:open-holes mission-status)
              (:hole-count mission-status)
              (:holes-open mission-status))]
    (and (number? n) (pos? n))))

(defn- registry-live-mission?
  [target]
  (try
    (let [open-missions-fn (requiring-resolve 'futon2.aif.mission-registry/open-missions)
          live-target-fn   (requiring-resolve 'futon2.aif.mission-registry/live-mission-target?)]
      (boolean (and open-missions-fn
                    live-target-fn
                    (live-target-fn (open-missions-fn) target))))
    (catch Throwable _
      false)))

(defn default-mission-status
  "Default mission status lookup. The futon2 registry currently establishes
   liveness; it does not expose open-hole counts, so the default result is
   intentionally not autonomous unless a richer caller supplies hole evidence."
  [target]
  {:open? (registry-live-mission? target)
   :open-hole-count nil})

(defn open-mission-with-holes?
  "True iff TARGET names an existing open mission with >= 1 open hole.
   CTX may provide :mission-status-fn returning a map with :open? and one of
   :open-hole-count/:open-holes/:hole-count/:holes-open."
  [target ctx]
  (let [mission-status-fn (or (:mission-status-fn ctx) default-mission-status)
        mission-status (mission-status-fn target)]
    (boolean (and (:open? mission-status)
                  (positive-open-hole-count? mission-status)))))

(defn autonomous-admissible?
  "Return true iff ACTION is safe to execute autonomously per WM guardrails.
   ACTION is the dT entry's :action. CTX accepts :mission-status-fn and
   :forbidden-path? injections for pure tests."
  [action ctx]
  (let [type (action-type action)
        forbidden-path? (or (:forbidden-path? ctx) default-forbidden-path?)]
    (boolean
     (and (contains? autonomous-action-types type)
          (not (contains? operator-only-action-types type))
          (not (marker-present? action))
          (not (target-forbidden? action forbidden-path?))
          (not (outward-op? action))
          (case type
            :open-mission (open-mission-with-holes? (action-target action) ctx)
            true)))))

(defn classify-action
  "Classify ACTION for the guarded WM selector.
   Returns :autonomous or :needs-operator."
  [action ctx]
  (if (autonomous-admissible? action ctx)
    :autonomous
    :needs-operator))
