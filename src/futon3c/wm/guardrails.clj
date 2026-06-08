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

(def pattern-warrants
  {:niche-deform
   {:pattern-id :aif/niche-construction
    :supporting-pattern-ids [:aif/admissibility]
    :warrant "this grows my own toolset / deforms the goal-structure — per the niche-construction rule you set, that's yours to authorize"
    :gap "greenlight the scope (or decline)"
    :unblock "Greenlight the scope, or decline."}

   :outward-irreversible
   {:pattern-id :orchestration/consent-gate
    :warrant "an outward, irreversible act — consent-gate wants your hand on it"
    :gap "approve the send"
    :unblock "Approve the send, or decline."}

   :unregistered-pursuit
   {:pattern-id :aif/admissibility
    :warrant "this pursues a capability not on your pre-registered ascent — admissibility gates pursuit, not discovery"
    :gap "register the capability, or decline"
    :unblock "Register the capability, or decline."}

   :open-mission-no-holes
   {:pattern-id :war-machine/advanceability
    :warrant "nothing concrete to advance this cycle — advanceability needs >=1 open hole"
    :gap "articulate the next hole / agree the gap, or it's not ready"
    :unblock "Articulate the next hole / agree the gap, or mark it not ready."}

   :operator-only
   {:pattern-id :orchestration/pattern-warranted-choice-point
    :supporting-pattern-ids [:orchestration/consent-gate]
    :warrant "operator-only by the consent posture"
    :gap "confirm"
    :unblock "Confirm."}})

(def hard-refuse-warrants
  {:forbidden-path
   {:warrant "protected boundary"
    :unblock "Declined: protected boundary."}})

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
  "Default mission status lookup through the futon2 registry."
  [target]
  (try
    (if-let [mission-status-fn (requiring-resolve 'futon2.aif.mission-registry/mission-status)]
      (mission-status-fn target)
      {:open? (registry-live-mission? target)
       :open-hole-count 0})
    (catch Throwable _
      {:open? false
       :open-hole-count 0})))

(defn open-mission-with-holes?
  "True iff TARGET names an existing open mission with >= 1 open hole.
   CTX may provide :mission-status-fn returning a map with :open? and one of
   :open-hole-count/:open-holes/:hole-count/:holes-open."
  [target ctx]
  (let [mission-status-fn (or (:mission-status-fn ctx) default-mission-status)
        mission-status (mission-status-fn target)]
    (boolean (and (:open? mission-status)
                  (positive-open-hole-count? mission-status)))))

(defn- registered-capability?
  [action ctx]
  (boolean
   (or (:pre-registered? action)
       (when-let [registered? (:registered-capability? ctx)]
         (registered? (:target action))))))

(defn guardrail-rule
  "Return the first guardrail rule that blocks ACTION, or nil when autonomous."
  [action ctx]
  (let [type (action-type action)
        target (action-target action)
        forbidden-path? (or (:forbidden-path? ctx) default-forbidden-path?)]
    (cond
      (target-forbidden? action forbidden-path?) :forbidden-path
      (and (= :pursue type) (not (registered-capability? action ctx))) :unregistered-pursuit
      (contains? operator-only-action-types type) :operator-only
      (marker-present? action) :niche-deform
      (outward-op? action) :outward-irreversible
      (not (contains? autonomous-action-types type)) :operator-only
      (and (= :open-mission type) (not (open-mission-with-holes? target ctx))) :open-mission-no-holes
      :else nil)))

(defn nag-warrant
  "Return the fillable operator warrant for ACTION, or nil for autonomous/hard-refused actions."
  [action ctx]
  (some-> (guardrail-rule action ctx) pattern-warrants))

(defn hard-refuse-warrant
  [action ctx]
  (some-> (guardrail-rule action ctx) hard-refuse-warrants))

(defn autonomous-admissible?
  "Return true iff ACTION is safe to execute autonomously per WM guardrails.
   ACTION is the dT entry's :action. CTX accepts :mission-status-fn and
   :forbidden-path? injections for pure tests."
  [action ctx]
  (nil? (guardrail-rule action ctx)))

(defn classify-action
  "Classify ACTION for the guarded WM selector.
   Returns :autonomous, :needs-operator, or :refused."
  [action ctx]
  (cond
    (autonomous-admissible? action ctx) :autonomous
    (hard-refuse-warrant action ctx) :refused
    :else :needs-operator))
