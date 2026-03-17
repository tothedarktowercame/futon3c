(ns futon3c.logic.obligation
  "Map structural-law violations into obligation records.

   The output records are intentionally compatible with the existing task-queue
   shape (`:id`, `:label`, `:priority`, `:source`, `:depends-on`) while also
   carrying structural-law metadata (`:domain-id`, `:violation-key`,
   `:actionability`, `:family`, `:payload`)."
  (:require [clojure.string :as str]))

(def ^:private known-actionabilities
  #{:auto-fixable :needs-review :informational})

(defn- tuple-label
  [prefix payload]
  (str prefix ": " (pr-str payload)))

(def ^:private violation-rules
  {[:agency :untyped-agents]
   {:actionability :auto-fixable
    :family :authorization-and-identity-discipline
    :priority :high
    :label-fn #(str "Repair missing typed agent identity for " %)
    :summary "Agency registry entry is missing a valid typed identity."}

   [:agency :duplicate-sessions]
   {:actionability :needs-review
    :family :authorization-and-identity-discipline
    :priority :high
    :label-fn (fn [[a1 a2 sid]]
                (str "Review duplicate session " sid " shared by " a1 " and " a2))
    :summary "Multiple agents claim the same live session continuity."}

   [:agency :proxy-agents]
   {:actionability :informational
    :family :authorization-and-identity-discipline
    :priority :low
    :label-fn (fn [[aid _origin]]
                (str "Proxy agent present in registry: " aid))
    :summary "Proxy agents are reported for visibility; presence alone is not a defect."}

   [:agency :route-inconsistencies]
   {:actionability :auto-fixable
    :family :status-discipline
    :priority :high
    :label-fn (fn [{:keys [agent-id route has-fn?]}]
                (str "Repair invoke-route mismatch for " agent-id
                     " (route " route ", local-fn? " has-fn? ")"))
    :summary "Agency invoke routing disagrees with the local invocation surface."}

   [:agency :agents-invoking]
   {:actionability :informational
    :family :status-discipline
    :priority :low
    :label-fn #(str "Agent currently invoking: " %)
    :summary "Current :invoking agents are surfaced for situational awareness."}

   [:agency :dead-end-peripherals]
   {:actionability :needs-review
    :family :graph-symmetry
    :priority :normal
    :label-fn #(str "Review dead-end peripheral topology for " %)
    :summary "Peripheral topology exposes an entry surface with no declared exit."}

   [:agency :entry-exit-asymmetry]
   {:actionability :needs-review
    :family :graph-symmetry
    :priority :normal
    :label-fn (fn [[from to]]
                (str "Review hop asymmetry from " from " to " to))
    :summary "A peripheral declares a hop that the target does not accept. The current ledger treats this as an architectural review question, not an automatic patch."}

   [:agency :invalid-observed-hops]
   {:actionability :needs-review
    :family :graph-symmetry
    :priority :normal
    :label-fn (fn [{:keys [from to session-id]}]
                (str "Review invalid observed hop " from " -> " to
                     " in session " session-id))
    :summary "Observed peripheral traffic violates the declared topology and needs architectural review."}

   [:tickle :unregistered-pages]
   {:actionability :auto-fixable
    :family :existence
    :priority :high
    :label-fn #(tuple-label "Repair page targeting unregistered agent" %)
    :summary "Tickle page event refers to an agent missing from the registry."}

   [:tickle :unroutable-pages]
   {:actionability :auto-fixable
    :family :status-discipline
    :priority :high
    :label-fn #(tuple-label "Repair unroutable page target" %)
    :summary "Tickle attempted to page an agent that is not currently routable."}

   [:tickle :pages-without-assignments]
   {:actionability :auto-fixable
    :family :dependency-satisfaction
    :priority :high
    :label-fn #(tuple-label "Repair page without assignment backing" %)
    :summary "Tickle page event lacks the assignable obligation pair that should justify it."}

   [:tickle :pages-without-authority]
   {:actionability :needs-review
    :family :authorization-and-identity-discipline
    :priority :high
    :label-fn #(tuple-label "Review page without authority" %)
    :summary "Tickle page event lacks the authority evidence required to justify dispatch."}

   [:tickle :orphan-escalations]
   {:actionability :auto-fixable
    :family :graph-symmetry
    :priority :high
    :label-fn #(tuple-label "Repair escalation without prior page" %)
    :summary "Escalation event is not backed by the required page event."}

   [:tickle :watchdog-pages-without-scans]
   {:actionability :auto-fixable
    :family :required-outputs
    :priority :normal
    :label-fn #(tuple-label "Repair watchdog page without scan evidence" %)
    :summary "Watchdog paging occurred without the scan evidence that should precede it."}

   [:tickle :watchdog-escalations-without-pages]
   {:actionability :auto-fixable
    :family :phase-ordering
    :priority :normal
    :label-fn #(tuple-label "Repair watchdog escalation without page" %)
    :summary "Watchdog escalation violated the expected scan -> page -> escalate progression."}

   [:tickle :stall-evidence-mismatches]
   {:actionability :needs-review
    :family :human-visible-inspectability
    :priority :normal
    :label-fn #(tuple-label "Review stall/evidence mismatch" %)
    :summary "Tickle's stall judgment disagrees with the visible evidence trail."}

   [:proof :asymmetric-edges]
   {:actionability :auto-fixable
    :family :graph-symmetry
    :priority :high
    :label-fn #(tuple-label "Repair proof DAG edge asymmetry" %)
    :summary "Proof unlock/dependency edges disagree about the same structural relation."}

   [:proof :dangling-refs]
   {:actionability :auto-fixable
    :family :existence
    :priority :high
    :label-fn #(tuple-label "Repair dangling proof ledger reference" %)
    :summary "Proof ledger references an item that does not exist."}

   [:proof :invalid-statuses]
   {:actionability :auto-fixable
    :family :status-discipline
    :priority :high
    :label-fn #(tuple-label "Repair invalid proof status" %)
    :summary "Proof ledger contains a status outside the allowed set."}

   [:proof :proved-without-analytical]
   {:actionability :needs-review
    :family :status-discipline
    :priority :high
    :label-fn #(str "Review proof claimed without analytical evidence for " %)
    :summary "A proof item is marked proved on numerical evidence alone."}

   [:proof :proved-with-unproved-deps]
   {:actionability :needs-review
    :family :dependency-satisfaction
    :priority :high
    :label-fn (fn [{:keys [item dep dep-status]}]
                (str "Review proved item " item " with unresolved dependency "
                     dep " (" dep-status ")"))
    :summary "A proof claim is ahead of its dependency chain and needs mathematical review."}

   [:proof :missing-phase-outputs]
   {:actionability :auto-fixable
    :family :required-outputs
    :priority :normal
    :label-fn (fn [{:keys [cycle phase missing]}]
                (str "Repair missing proof outputs for " cycle " at " phase
                     " " (pr-str missing)))
    :summary "Proof cycle advanced without recording the outputs required for an earlier phase."}

   [:proof :mode-violations]
   {:actionability :needs-review
    :family :phase-ordering
    :priority :normal
    :label-fn #(tuple-label "Review proof mode transition" %)
    :summary "Proof mode advanced beyond the falsification boundary without the required precondition."}

   [:proof :routes-without-obstruction]
   {:actionability :needs-review
    :family :failure-locality
    :priority :normal
    :label-fn #(str "Review failed proof route without obstruction: " %)
    :summary "A failed proof route did not record the structural obstruction that should explain it."}

   [:mission :missing-blockers]
   {:actionability :auto-fixable
    :family :existence
    :priority :high
    :label-fn #(tuple-label "Repair mission cycle blocker reference" %)
    :summary "Mission cycle points at a blocker that is absent from the obligation ledger."}

   [:mission :invalid-statuses]
   {:actionability :auto-fixable
    :family :status-discipline
    :priority :high
    :label-fn #(tuple-label "Repair invalid mission obligation status" %)
    :summary "Mission obligation uses a status outside the allowed set."}

   [:mission :done-with-assertion-only]
   {:actionability :needs-review
    :family :status-discipline
    :priority :high
    :label-fn #(str "Review mission obligation marked done on assertion only: " %)
    :summary "Mission obligation was marked done using only assertion evidence."}

   [:mission :phase-order-violations]
   {:actionability :auto-fixable
    :family :phase-ordering
    :priority :normal
    :label-fn (fn [{:keys [cycle current-phase]}]
                (str "Repair mission phase ordering for " cycle
                     " at " current-phase))
    :summary "Mission cycle's completed-phase prefix does not match its current phase."}

   [:mission :missing-phase-outputs]
   {:actionability :auto-fixable
    :family :required-outputs
    :priority :normal
    :label-fn (fn [{:keys [cycle phase missing]}]
                (str "Repair mission outputs for " cycle " at " phase
                     " " (pr-str missing)))
    :summary "Mission cycle advanced without the required phase outputs."}

   [:codex :jobs-for-unregistered-agents]
   {:actionability :auto-fixable
    :family :existence
    :priority :high
    :label-fn #(tuple-label "Repair Codex job for unregistered agent" %)
    :summary "Canonical Codex job ledger references an agent absent from the registry."}

   [:codex :running-jobs-on-idle-agents]
   {:actionability :auto-fixable
    :family :status-discipline
    :priority :high
    :label-fn #(tuple-label "Repair running-job / idle-agent contradiction" %)
    :summary "Codex runtime reports running work while the registry says the agent is idle."}

   [:codex :running-count-mismatches]
   {:actionability :auto-fixable
    :family :status-discipline
    :priority :high
    :label-fn #(tuple-label "Repair registry running-count mismatch" %)
    :summary "Registry aggregate counts disagree with the implied running status."}

   [:codex :orphan-announcements]
   {:actionability :auto-fixable
    :family :cross-store-agreement
    :priority :high
    :label-fn #(tuple-label "Repair orphan Codex announcement" %)
    :summary "Public Codex announcement is not backed by the canonical job ledger."}

   [:codex :announcement-agent-mismatches]
   {:actionability :needs-review
    :family :cross-store-agreement
    :priority :high
    :label-fn #(tuple-label "Review announcement/ledger agent mismatch" %)
    :summary "Public acceptance metadata disagrees with the canonical job ledger about the responsible agent."}

   [:codex :running-session-mismatches]
   {:actionability :needs-review
    :family :cross-store-agreement
    :priority :high
    :label-fn #(tuple-label "Review Codex session continuity mismatch" %)
    :summary "Running work disagrees across stores about the active session continuity token."}})

(defn rule-for
  [domain-id violation-key]
  (or (get violation-rules [domain-id violation-key])
      {:actionability :needs-review
       :family :unclassified
       :priority :normal
       :label-fn #(str "Review unclassified structural-law violation "
                       (name domain-id) "/" (name violation-key)
                       ": " (pr-str %))
       :summary "Violation key is not yet classified. Surface it explicitly rather than silently dropping it."
       :classification-source :fallback}))

(defn- ensure-actionability
  [rule]
  (let [actionability (:actionability rule)]
    (when-not (contains? known-actionabilities actionability)
      (throw (ex-info "Unknown obligation actionability"
                      {:rule rule :known known-actionabilities})))
    rule))

(defn dispatchable?
  [obligation]
  (= :auto-fixable (:actionability obligation)))

(defn obligation->task
  "Return a task-queue compatible map for auto-fixable obligations, else nil."
  [obligation]
  (when (dispatchable? obligation)
    (select-keys obligation
                 [:id :label :priority :source :depends-on
                  :domain-id :violation-key :actionability :family :payload])))

(defn- stable-id
  [domain-id violation-key idx]
  (format "INV-%s-%s-%03d"
          (-> domain-id name)
          (-> violation-key name (str/replace "_" "-"))
          idx))

(defn- obligation-record
  [domain-id violation-key idx payload]
  (let [{:keys [actionability family priority label-fn summary classification-source]
         :or {classification-source :explicit}}
        (ensure-actionability (rule-for domain-id violation-key))]
    {:id (stable-id domain-id violation-key idx)
     :label (label-fn payload)
     :priority priority
     :source (str "structural-law/" (name domain-id))
     :depends-on #{}
     :domain-id domain-id
     :violation-key violation-key
     :actionability actionability
     :family family
     :payload payload
     :summary summary
     :dispatchable? (= :auto-fixable actionability)
     :classification-source classification-source}))

(defn report->obligations
  "Expand one invariant-runner domain report into obligation records.

   Dormant domains produce no obligations."
  [{:keys [domain-id state violations]}]
  (if (= :dormant state)
    []
    (mapcat (fn [violation-key]
              (map-indexed (fn [idx payload]
                             (obligation-record domain-id violation-key (inc idx) payload))
                           (get violations violation-key [])))
            (sort-by name (keys violations)))))

(defn reports->obligations
  [reports]
  (vec (mapcat report->obligations reports)))

(defn group-by-actionability
  [obligations]
  {:auto-fixable (vec (filter #(= :auto-fixable (:actionability %)) obligations))
   :needs-review (vec (filter #(= :needs-review (:actionability %)) obligations))
   :informational (vec (filter #(= :informational (:actionability %)) obligations))})

(defn dispatchable-tasks
  [obligations]
  (->> obligations
       (filter dispatchable?)
       (mapv obligation->task)))
