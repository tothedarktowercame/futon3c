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
            [futon3c.peripheral.tools :as tools]))

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
