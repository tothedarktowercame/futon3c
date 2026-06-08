(ns futon3c.peripheral.war-machine-pilot
  "War Machine Pilot peripheral — SPIKE STAGE.

   M-war-machine-pilot VERIFY phase (2026-05-24).
   The smallest viable cycle-peripheral that instantiates the generic
   cycle.clj engine with war-machine-pilot domain configuration.

   This SPIKE validates:
   - C1 (envelope file exists) — this file + war_machine_pilot_shapes.clj
   - cycle.clj engine accepts the pilot domain config
   - `cycle/valid-domain-config?` returns true for `pilot-domain-config`
   - `make-pilot` returns a peripheral without runtime error

   This SPIKE does NOT yet validate:
   - C2 (agent inhabits + emits bell) — needs Agency registration
   - C3 (Playwright reach) — needs :playwright-run tool wiring
   - C4 (CLJ backend reach) — needs :drawbridge-eval tool wiring
   - C5 (substrate-read works in practice) — needs :anchors-read backend wiring
   - C6/C7/C8 — needs write tools + consent-gate machinery

   These deferred validations land in INSTANTIATE per the mission's
   exit-criterion check (M-war-machine-pilot.md VERIFY §Decision log).

   Cross-refs:
   - DERIVE §Choice 2 (clone-from-Mission)
   - mission.clj — the clone-from shape donor
   - cycle.clj — the generic engine this peripheral instantiates
   - war_machine_pilot_shapes.clj — domain config field declarations"
  (:require [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.war-machine-pilot-shapes :as pps]
            [futon3c.peripheral.war-machine-pilot-backend :as pb]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.wm.guardrails :as guardrails]
            [futon3c.wm.needs-you :as needs-you]))

;; =============================================================================
;; Domain state initialization
;; =============================================================================

(defn- state-init
  "Initialize pilot-specific state fields from context.
   SPIKE STAGE: minimal — just records the inhabitation moment."
  [_context]
  {:pilot-inhabited-at (str (java.time.Instant/now))})

;; =============================================================================
;; Fruit and exit context
;; =============================================================================

(defn- fruit
  "Extract fruit from pilot session state.
   SPIKE STAGE: reports the anchors-summary the :observe phase produced."
  [state]
  {:pilot-inhabited-at (:pilot-inhabited-at state)
   :anchors-summary    (:anchors-summary state)
   :cycles-completed   (:cycles-completed state)
   :final-phase        (:current-phase state)})

(defn- exit-context
  "Extract exit context for hop/resume.
   SPIKE STAGE: minimal hand-off envelope."
  [state]
  {:session-id        (:session-id state)
   :pilot-inhabited-at (:pilot-inhabited-at state)})

;; =============================================================================
;; Domain config — the 象 entering the cycle machine
;; =============================================================================

(def pilot-domain-config
  "CycleDomainConfig for the War Machine Pilot peripheral.
   SPIKE STAGE — minimal viable config that passes `cycle/valid-domain-config?`."
  {:domain-id          :war-machine-pilot
   :phase-order        pps/phase-order
   :phase-tools        pps/phase-allowed-tools
   :setup-tools        pps/setup-tools
   :tool-ops           pps/pilot-tool-operation-kinds
   :required-outputs   pps/phase-required-outputs
   :cycle-begin-tool   :cycle-begin
   :cycle-advance-tool :cycle-advance
   :state-init-fn      state-init
   :fruit-fn           fruit
   :exit-context-fn    exit-context})

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-pilot
  "Create a War Machine Pilot peripheral from optional backend.
   Uses the generic cycle machine with pilot-domain configuration.

   PHASE 1 OF INSTANTIATE (2026-05-24): the spike's mock-backend stance is
   replaced with PilotBackend wrapping. The PilotBackend handles `:anchors-read`
   against the live wm-ui-anchors.edn substrate; all other tools delegate to
   the inner backend (mock or real)."
  ([] (make-pilot (tools/make-mock-backend)))
  ([backend]
   (cycle/make-cycle-peripheral pilot-domain-config (pb/make-pilot-backend backend)))
  ([spec backend]
   (cycle/make-cycle-peripheral pilot-domain-config spec (pb/make-pilot-backend backend))))

;; =============================================================================
;; Spike validation helper (callable via Drawbridge nREPL)
;; =============================================================================

;; =============================================================================
;; Phase 1 INSTANTIATE — end-to-end observe-cycle runner
;; =============================================================================

(defn run-observe-cycle
  "Phase 1 INSTANTIATE end-to-end runner.
   Inhabits the pilot envelope, runs one :observe cycle (which reads
   wm-ui-anchors.edn via :anchors-read), advances to :completed, and stops.
   Returns the fruit. Read-only — no substrate writes."
  ([] (run-observe-cycle {:session-id (str "pilot-" (java.util.UUID/randomUUID))}))
  ([context]
   (let [cycle-id (str (java.util.UUID/randomUUID))
         mock-results {:cycle-begin   {:cycle/id cycle-id :cycle/phase :observe}
                       :cycle-advance {:cycle/phase :completed}}
         pilot (make-pilot (tools/make-mock-backend mock-results))
         start-r (runner/start pilot context)]
     (if-not (:ok start-r)
       {:phase :start :error start-r}
       (let [r1 (runner/step pilot (:state start-r)
                             {:tool :cycle-begin :args []})]
         (if-not (:ok r1)
           {:phase :cycle-begin :error r1}
           (let [r2 (runner/step pilot (:state r1)
                                 {:tool :anchors-read :args []})]
             (if-not (:ok r2)
               {:phase :anchors-read :error r2}
               (let [state-with-summary (assoc (:state r2) :anchors-summary (:result r2))
                     ;; Phase 2: emit completion-bell BEFORE cycle-advance to capture inhabitation evidence
                     summary (:result r2)
                     bell-prompt (str "[pilot/observe-cycle complete] "
                                      "anchors=" (:total summary)
                                      " addressed=" (:addressed-count summary)
                                      " open=" (count (:open-substantive summary))
                                      " coherence-rows=" (:coherence-row-count summary)
                                      " recent=" (vec (:recent-addressed summary)))
                     r-bell (runner/step pilot state-with-summary
                                         {:tool :bell-emit
                                          :args [{:agent-id "claude-10"
                                                  :prompt bell-prompt
                                                  :pilot-event :pilot/observe-cycle-complete}]})
                     state-after-bell (if (:ok r-bell) (:state r-bell) state-with-summary)
                     r3 (runner/step pilot state-after-bell
                                     {:tool :cycle-advance :args []})]
                 (if-not (:ok r3)
                   {:phase :cycle-advance :error r3 :bell-result r-bell}
                   (let [stop-r (runner/stop pilot (:state r3) :completed)]
                     {:phase           :completed
                      :ok?             (boolean (:ok stop-r))
                      :fruit           (:fruit stop-r)
                      :exit-context    (:context stop-r)
                      :anchors-summary (:result r2)
                      :completion-bell (cond
                                         (not (:ok r-bell))      {:emitted? false :error r-bell}
                                         :else                   {:emitted? true
                                                                  :job-id (get-in r-bell [:result :body :job-id])
                                                                  :payload-schema (get-in r-bell [:result :payload-schema])})})))))))))))

(defn spike-check
  "Spike-validation entrypoint — exercised by M-war-machine-pilot VERIFY.
   Returns {:valid-config? bool :make-pilot-ok? bool :error nil-or-string}.

   Call via Drawbridge nREPL:
     (require '[futon3c.peripheral.war-machine-pilot :as wmp])
     (wmp/spike-check)"
  []
  (let [valid? (cycle/valid-domain-config? pilot-domain-config)
        make-result (try (make-pilot)
                         (catch Throwable t {:error t}))
        make-ok? (and (not (instance? Throwable make-result))
                      (not (:error make-result)))]
    {:valid-config?     valid?
     :make-pilot-ok?    make-ok?
     :spike-error       (when (instance? Throwable make-result)
                          (.getMessage ^Throwable make-result))
     :spike-stage       :verify
     :mission           "M-war-machine-pilot"
     :verified-criteria (cond-> []
                          valid?    (conj :C1-envelope-file-exists-and-config-valid)
                          make-ok?  (conj :make-pilot-returns-without-error))
     :deferred-criteria [:C2-agency-registration-and-inhabitation-bell
                         :C3-playwright-reach
                         :C4-clj-backend-reach
                         :C5-substrate-read-in-practice
                         :C6-first-ui-improvement
                         :C7-wm-i4-preservation-at-runtime
                         :C8-consent-gate-exercised]}))

;; =============================================================================
;; INSTANTIATE Phase 2 — LIVE REPL cycle (differential-operator spec)
;;
;; Implements the four-turn operator from holes/specs/repl.spec.edn against the
;; LIVE War Machine, emitting a verifiable γ frame via futon3c.aif.repl-trace:
;;   READ  : O(1) cached judgement (pb/wm-api-query) → ranked-actions = dT
;;   EVAL  : v = top action; predicted = its G-total; mint cg-id
;;   PRINT : supervised-proposal — artefact = the cg intent-handshake (the
;;           explored candidate); substantive execution deferred to operator
;;           merge (gate-at-merge). No substrate mutation; operator bell OFF by
;;           default so the test activates no other agent.
;;   LOOP  : request-tick! (async, never the blocking tick!) then re-read;
;;           realised = post-tick G-total of the chosen target; pred-error auto.
;; Two-phase (begin!/close!) so we never hold an eval thread across the tick.
;; requiring-resolve keeps the ns :require form (and reload surface) unchanged.
;; =============================================================================

(defonce ^:private !live-cycle-runs (atom {}))

(defn- live-judgement []
  (let [r (pb/wm-api-query {})]
    (if (:ok r)
      (get-in r [:result :judgement])
      (throw (ex-info "live READ failed (wm-api-query)" {:r r})))))

(defn- judgement->dT
  "Project a WM judgement's ranked-actions into a dT-snapshot
   [{:action {:type :target} :g-total :rank} …] — the differential dT_p."
  [judgement]
  (->> (:ranked-actions judgement)
       (mapv (fn [e]
               (let [action (:action e)]
                 {:action (assoc action :type (some-> (:type action) keyword))
                  :g-total (:G-total e)
                  :rank    (:rank e)})))))

(defn- guarded-selection
  [dT ctx]
  (let [classified (mapv (fn [entry]
                           (let [action (:action entry)
                                 rule (guardrails/guardrail-rule action ctx)
                                 warrant (guardrails/nag-warrant action ctx)]
                             (cond-> (assoc entry :guardrails/classification
                                            (guardrails/classify-action action ctx))
                               rule (assoc :guardrails/rule rule)
                               warrant (assoc :guardrails/pattern-warrant warrant))))
                         dT)
        autonomous (first (filter #(= :autonomous (:guardrails/classification %))
                                  classified))
        stepped-past (if autonomous
                       (->> classified
                            (take-while #(not= autonomous %))
                            (filter #(= :needs-operator (:guardrails/classification %)))
                            vec)
                       (filterv #(= :needs-operator (:guardrails/classification %))
                                classified))]
    {:autonomous autonomous
     :stepped-past stepped-past}))

(defn begin-live-cycle!
  "READ → EVAL → PRINT(proposal) → request async tick. Fast, non-blocking, no
   substrate mutation. Stashes full begin-state by run-id; returns a summary."
  ([] (begin-live-cycle! {}))
  ([{:keys [agent v-attribution emit-bell? tick? mode guardrails? guardrails-ctx needs-you-path needs-you-top-k]
     :or {agent "claude-2" v-attribution :pilot-autonomous emit-bell? false
          tick? true mode :supervised-proposal}}]
   (let [run-id (str "live-" (java.util.UUID/randomUUID))
         j      (live-judgement)
         dT     (judgement->dT j)
         guarded (when guardrails?
                   (guarded-selection dT (or guardrails-ctx {})))
         top    (if guardrails?
                  (:autonomous guarded)
                  (first dT))]
     (if (nil? top)
       (if guardrails?
         (let [items (mapv #(needs-you/action->needs-you-item % run-id)
                           (:stepped-past guarded))
               emitted (needs-you/emit-needs-you!
                        items
                        (cond-> {}
                          needs-you-path (assoc :path needs-you-path)
                          needs-you-top-k (assoc :top-k needs-you-top-k)))]
           {:ok false
            :reason :no-autonomous-action
            :n-ranked (count dT)
            :needs-you-emitted (:emitted-count emitted)
            :needs-you-path (:path emitted)})
         {:ok false :error "no ranked-actions in live judgement"})
       (let [v         (:action top)
             predicted (:g-total top)
             stepped-past (if guardrails? (:stepped-past guarded) [])
             emitted (when guardrails?
                       (needs-you/emit-needs-you!
                        (mapv #(needs-you/action->needs-you-item % run-id) stepped-past)
                        (cond-> {}
                          needs-you-path (assoc :path needs-you-path)
                          needs-you-top-k (assoc :top-k needs-you-top-k))))
             cg-id     (if emit-bell?
                         (get-in (pb/consent-gate-emit
                                  {:intent (str "REPL EVAL: engage " (pr-str v))
                                   :scope {:action v :mode :supervised-proposal :wm-mode (:mode j)}
                                   :constraints ["supervised-proposal: no mutation; operator merges"]
                                   :success-criteria ["γ frame emitted" "verifier conforms"]})
                                 [:result :consent-gate-event-id])
                         (str "cg-" (java.util.UUID/randomUUID)))
             tick-before (:tick-count ((requiring-resolve 'futon3c.wm.scheduler/status)))
             tick        (when tick? ((requiring-resolve 'futon3c.wm.scheduler/request-tick!)))
             begin {:ok true :run-id run-id :agent agent :v-attribution v-attribution
                    :wm-mode (:mode j) :mode mode
                    :pre {:dT-snapshot dT :v v :predicted-discharge predicted}
                    :cg-id cg-id
                    :artefact {:kind (if (= mode :substantive)
                                       :substantive-action
                                       :consent-gate-intent-handshake)
                               :cg-id cg-id :proposed-action v
                               :bell-emitted? (boolean emit-bell?)}
                    :tick-before tick-before :tick tick}
             result {:ok true :run-id run-id :wm-mode (:mode j) :mode mode :n-ranked (count dT)
                     :v v :predicted-discharge predicted :cg-id cg-id
                     :tick-before tick-before :tick-queued tick}]
         (swap! !live-cycle-runs assoc run-id begin)
         (cond-> result
           guardrails? (assoc :needs-you-emitted (:emitted-count emitted)
                              :needs-you-path (:path emitted))))))))

(defn close-live-cycle!
  "LOOP: re-read the (post-tick) judgement; realised = post G-total of the
   chosen target (or predicted if the target was resolved/absent). Build the
   turn-record, frame it (envelope + γ), persist, and return the measurement."
  [run-id]
  (if-let [b (get @!live-cycle-runs run-id)]
    (let [post-j     (live-judgement)
          post-dT    (judgement->dT post-j)
          v          (get-in b [:pre :v])
          predicted  (get-in b [:pre :predicted-discharge])
          target     (:target v)
          post-entry (first (filter #(= target (get-in % [:action :target])) post-dT))
          realised   (if post-entry (:g-total post-entry) predicted)
          pre-top    (get-in b [:pre :dT-snapshot 0 :action :target])
          post-top   (get-in post-dT [0 :action :target])
          tr ((requiring-resolve 'futon3c.aif.repl-trace/turn-record)
              {:step 0 :p "pre-tick"
               :dT-snapshot (get-in b [:pre :dT-snapshot])
               :v v :v-attribution (:v-attribution b)
               :predicted-discharge predicted
               :cg-id (:cg-id b) :artefact (:artefact b)
               :delta-grad? false
               :p' "post-tick" :realised-discharge realised})
          ;; LOOP :autonomy — auto-mine learning from the post-tick judgement
          ;; (futon3c.aif.loop-learning): patterns from REPL-cycle structure +
          ;; sorries from WM gap-signals minus registry-tracked. :auto-mined.
          open-ids (try (mapv :id ((requiring-resolve 'futon2.aif.sorry-registry/open-sorrys)))
                        (catch Throwable _ []))
          learning ((requiring-resolve 'futon3c.aif.loop-learning/loop-learning-pass)
                    {:judgement post-j :open-sorry-ids open-ids})
          frame ((requiring-resolve 'futon3c.aif.repl-trace/frame)
                 {:run-id run-id :agent (:agent b)
                  :date (subs (str (java.time.Instant/now)) 0 10)}
                 [tr] learning)
          frame+ (assoc frame
                        :wm-mode (:wm-mode b) :mode (:mode b)
                        :tick-before (:tick-before b)
                        :tick-after (:tick-count ((requiring-resolve 'futon3c.wm.scheduler/status))))
          path  ((requiring-resolve 'futon3c.aif.repl-trace/write-frame!) frame+ "data/repl-traces")]
      {:ok true :frame-path path :run-id run-id
       :predicted predicted :realised realised
       :prediction-error (Math/abs (double (- realised predicted)))
       :top-shift? (not= pre-top post-top) :pre-top pre-top :post-top post-top})
    {:ok false :error (str "unknown run-id " run-id)}))
