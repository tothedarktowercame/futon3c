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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.peripheral.cycle :as cycle]
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

(def ^:dynamic *live-runs-dir*
  "Where begin-state persists. Dynamic so TESTS bind it to a temp dir —
   fixture begins must never write into the real traces dir (they polluted
   the calibration evidence on 2026-06-11: test runs left sorry/foo begin
   files that the evidence reader ingested)."
  (str (System/getProperty "user.home") "/code/futon3c/data/repl-traces"))

(defn- begin-state-path [run-id]
  (str *live-runs-dir* "/" run-id ".begin.edn"))

(defn- persist-begin-state!
  "Durability for the begin→close window (Turn-3 finding, 2026-06-10): the
   atom alone dies with its process — a begin staged from one JVM (e.g. a
   CLI run) is invisible to a close in another, and 'unknown run-id' eats
   the DOCUMENT stage. Best-effort: a persist failure must not break begin."
  [run-id begin]
  (try
    (io/make-parents (begin-state-path run-id))
    (spit (begin-state-path run-id)
          (binding [*print-length* nil *print-level* nil]
            (pr-str begin)))
    (catch Throwable _ nil)))

(defn- recover-begin-state
  "Fallback for close-live-cycle! when the atom misses: read the persisted
   begin-state from disk. Returns nil when absent/unreadable."
  [run-id]
  (try
    (let [f (io/file (begin-state-path run-id))]
      (when (.exists f)
        (edn/read-string (slurp f))))
    (catch Throwable _ nil)))

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
                  ;; dual-prediction logging: frozen constant-model counterfactual
                  :g-constant (:G-constant e)
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
   substrate mutation. Stashes full begin-state by run-id; returns a summary.

   Guardrails ride BY DEFAULT (pilot cycle #1, 2026-06-10): the selection
   delegates to wm/guardrails — operator-only and unearnable actions are
   stepped past and NAGged via wm/needs-you, and the chosen v is the first
   :autonomous (e.g. bounded advancement of an open mission with open holes).
   Without this the default flight path was `(first dT)` with no
   classification — the pilot faced operator-only actions with no executor
   and had to hold by hand. Pass :guardrails? false for a raw field read."
  ([] (begin-live-cycle! {}))
  ([{:keys [agent v-attribution emit-bell? tick? mode guardrails? guardrails-ctx needs-you-path needs-you-top-k
            target action-type]
     :or {agent "claude-2" v-attribution :pilot-autonomous emit-bell? false
          tick? true mode :supervised-proposal guardrails? true}}]
   (let [run-id (str "live-" (java.util.UUID/randomUUID))
         j      (live-judgement)
         dT     (judgement->dT j)
         ;; CHOSEN-V (cycle-5 apparatus, 2026-06-11): :target selects a
         ;; SPECIFIC ranked entry instead of the guarded top. Guardrails are
         ;; STILL CONSULTED and the classification recorded — but an
         ;; operator-directed choice proceeds even when :needs-operator,
         ;; because the operator's direction IS the consent the gate exists
         ;; to obtain (consent satisfied upstream, not bypassed). The chosen
         ;; target must exist in the live differential — predicted-G must be
         ;; the field's own number, never an invented one.
         ;; Car-3 seam-a: disambiguate by (:type, :target), not :target alone — else an
         ;; :apply-cascade collides with a same-:target :advance-mission picked first.
         chosen (when target
                  (first (filter #(and (= target (get-in % [:action :target]))
                                       (or (nil? action-type)
                                           (= action-type (get-in % [:action :type])))) dT)))
         _ (when (and target (nil? chosen))
             (throw (ex-info "chosen :target not in the live differential — predicted-G must come from the field"
                             {:target target :n-ranked (count dT)})))
         chosen (when chosen
                  (let [ctx (or guardrails-ctx {})]
                    (assoc chosen
                           :guardrails/classification (guardrails/classify-action (:action chosen) ctx)
                           :guardrails/rule (guardrails/guardrail-rule (:action chosen) ctx))))
         v-attribution (if target :operator-directed v-attribution)
         guarded (when (and guardrails? (not target))
                   (guarded-selection dT (or guardrails-ctx {})))
         top    (cond
                  target      chosen
                  guardrails? (:autonomous guarded)
                  :else       (first dT))]
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
                                   :success-criteria ["γ frame emitted" "verifier conforms"]
                                   ;; Car-3 seam-b: thread the chosen action's act-gate so the
                                   ;; gate records ΔF∧ΔG + :gate-verdict (nil for non-apply-cascade v).
                                   :act-gate (:act-gate v)})
                                 [:result :consent-gate-event-id])
                         (str "cg-" (java.util.UUID/randomUUID)))
             tick-before (:tick-count ((requiring-resolve 'futon3c.wm.scheduler/status)))
             tick        (when tick? ((requiring-resolve 'futon3c.wm.scheduler/request-tick!)))
             begin {:ok true :run-id run-id :agent agent :v-attribution v-attribution
                    ;; flight-record grounds (M-first-flights step 2): the
                    ;; begin instant and the scan freshness, recorded not
                    ;; reconstructed (the stale-begin confound is checkable
                    ;; only if these are data)
                    :begin-at (str (java.time.Instant/now))
                    :scan-as-of (some-> (:as-of j) str)
                    :wm-mode (:mode j) :mode mode
                    :pre {:dT-snapshot dT :v v :predicted-discharge predicted
                          :predicted-constant (:g-constant top)}
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
         (persist-begin-state! run-id begin)
         (cond-> result
           guardrails? (assoc :needs-you-emitted (:emitted-count emitted)
                              :needs-you-path (:path emitted))
           target (assoc :guardrails/classification (:guardrails/classification top)
                         :guardrails/rule (:guardrails/rule top)
                         :v-attribution v-attribution)))))))

(def ^:private pilots-log-path
  (str (System/getProperty "user.home") "/code/futon3c/holes/PILOTS-LOG.md"))

(defn log-pilot-turn!
  "DOCUMENT stage of the LOOP turn: append a human-readable entry to the Pilot's Log
   (PILOTS-LOG.md, newest at top). Mechanical facts auto-fill from the cycle; the
   inhabitant supplies the semantic account via :document {:did :found :pur}. Best-effort —
   a log failure must never break a turn. Returns {:turn n :path p} or nil.
     turn-data: {:agent :mode :recommendation {:target :g :rationale}
                 :cascade {:patterns :wholeness} :measurement {:predicted :realised :top-shift? :pre-top :post-top}
                 :document {:did :found :pur}}"
  [{:keys [agent mode recommendation cascade measurement document]}]
  (try
    (let [path pilots-log-path
          text (slurp path)
          n (->> (re-seq #"(?m)^## Turn (\d+)" text)
                 (map (comp #(Integer/parseInt %) second))
                 (reduce max 0) inc)
          date (subs (str (java.time.Instant/now)) 0 10)
          {:keys [target g]} recommendation
          {:keys [patterns wholeness]} cascade
          {:keys [predicted realised top-shift? pre-top post-top]} measurement
          {:keys [did found pur]} document
          entry (str "## Turn " n " — " date " (" (or agent "pilot") ", " (name (or mode :supervised)) ")\n\n"
                     "**READ.** WM recommended `" target "`"
                     (when g (format " (G=%.2f)" (double g))) ".\n"
                     (when (seq patterns)
                       (str "Cascade" (when wholeness (format " (wholeness=%.2f)" (double wholeness))) ": "
                            (str/join " · " patterns) ".\n"))
                     "\n**EVAL / DID.** " (or did "_(not recorded — mechanical turn)_") "\n"
                     "\n**PRINT / FOUND.** " (or found "_(not recorded)_")
                     (when (and predicted realised)
                       (format "  _[predicted G=%.2f, realised G=%.2f%s]_"
                               (double predicted) (double realised)
                               (if top-shift? (str ", top-shift " pre-top "->" post-top) "")))
                     "\n\n**PUR.** " (or pur "_(not recorded)_") " Mode: " (name (or mode :supervised)) ".\n")
          idx (str/index-of text "## Turn ")
          [head tail] (if idx [(subs text 0 idx) (subs text idx)] [text ""])
          new-text (str (str/trimr head) "\n\n" entry "\n" tail)]
      (spit path new-text)
      {:turn n :path path})
    (catch Throwable _ nil)))

(defn close-live-cycle!
  "LOOP: re-read the (post-tick) judgement; realised = post G-total of the
   chosen target (or predicted if the target was resolved/absent). Build the
   turn-record, frame it (envelope + γ), persist, log the Pilot's-Log turn (DOCUMENT
   stage — pass the semantic account via opts `:document {:did :found :pur}`), and
   return the measurement.

   REALISED-ON-MERGE (closes :sorry/wm-realised-on-merge-binding): pass
   `:executed? true :evidence-ref <witness>` when the cycle's action actually
   EXECUTED (substantive mode / operator merge) — the realised-G then comes
   from a field the action genuinely moved, the γ pair is tagged
   :independent? true, and the calibration verdict may count it. Independence
   is a CLAIM that needs a WITNESS: :executed? without :evidence-ref (commit
   sha, CH2 event, mission-status change …) throws — anti-laundering, same
   rule as CH2's payload. Proposal-mode closes (the default) stay untagged
   and can never move the verdict."
  ([run-id] (close-live-cycle! run-id {}))
  ([run-id {:keys [document executed? evidence-ref realised-read flight]}]
  (when (and executed? (not evidence-ref))
    (throw (ex-info ":executed? true requires :evidence-ref — independence is a claim that needs a witness (no payload, no discharge)"
                    {:run-id run-id})))
  ;; realised-read protocol (cycles 5-7 finding: transient-vs-settled timing
  ;; was undefined, making pairs incomparable — cycle 6 caught a spike).
  ;; Pilot attests the read point: :settled = two consecutive scans agree
  ;; within epsilon before close; :transient = first-post-commit read.
  ;; Calibration excludes :transient from the verdict.
  (when (and realised-read (not (#{:settled :transient} realised-read)))
    (throw (ex-info ":realised-read must be :settled or :transient" {:got realised-read})))
  (if-let [b (or (get @!live-cycle-runs run-id)
                 (recover-begin-state run-id))]
    (let [post-j     (live-judgement)
          post-dT    (judgement->dT post-j)
          v          (get-in b [:pre :v])
          predicted  (get-in b [:pre :predicted-discharge])
          target     (:target v)
          v-type     (:type v)
          ;; Car-3 seam-a: match the post-entry by (:type, :target) — else realised is
          ;; mis-measured against a same-:target :advance-mission instead of the chosen v.
          post-entry (first (filter #(and (= target (get-in % [:action :target]))
                                          (= v-type (get-in % [:action :type]))) post-dT))
          realised   (if post-entry (:g-total post-entry) predicted)
          ;; A vanished target means the discharge SUCCEEDED but realised-G
          ;; has no measurement — the fallback copies predicted, which would
          ;; fabricate a perfect prediction-error of 0.0. Tag the source so
          ;; calibration can exclude fallback pairs from the verdict
          ;; (a censored observation is not evidence of calibration).
          realised-source (if post-entry :measured :target-absent-fallback)
          ;; Option C (2026-06-11): on executed cycles also record the WHOLE
          ;; differential's movement — observational only (attribution on a
          ;; live multi-agent stack is unsolved; this accumulates the data to
          ;; design field-delta realised-semantics properly).
          g-sum (fn [entries] (reduce + 0.0 (keep :g-total entries)))
          field-delta (when executed?
                        {:pre-total (g-sum (get-in b [:pre :dT-snapshot]))
                         :post-total (g-sum post-dT)
                         :delta (- (g-sum post-dT)
                                   (g-sum (get-in b [:pre :dT-snapshot])))
                         :semantics :observational-not-verdict-counted})
          pre-top    (get-in b [:pre :dT-snapshot 0 :action :target])
          post-top   (get-in post-dT [0 :action :target])
          tr ((requiring-resolve 'futon3c.aif.repl-trace/turn-record)
              (cond-> {:step 0 :p "pre-tick"
                       :dT-snapshot (get-in b [:pre :dT-snapshot])
                       :v v :v-attribution (:v-attribution b)
                       :predicted-discharge predicted
                       :predicted-constant (get-in b [:pre :predicted-constant])
                       :cg-id (:cg-id b) :artefact (:artefact b)
                       :delta-grad? false
                       :p' "post-tick" :realised-discharge realised}
                executed? (assoc :independent? true
                                 :evidence-ref evidence-ref
                                 :realised-source realised-source
                                 :field-delta field-delta)
                (and executed? realised-read) (assoc :realised-read realised-read)))
          ;; the merge itself is an out-of-band gradient event — record it
          ;; in the discipline channel (best-effort; never breaks a close).
          ;; ONE instant shared with the flight record's oob organ, so the
          ;; two stores agree to the nanosecond (grounds are exact where
          ;; exact values exist — the witness-flight conformance lesson)
          merge-at (when executed? (str (java.time.Instant/now)))
          _ (when executed?
              (try ((requiring-resolve 'futon3c.aif.discipline-events/append-event!)
                    {:discipline/event :operator-merge
                     :run-id run-id
                     :at merge-at
                     :action v
                     :predicted predicted
                     :note (str "executed cycle; evidence: " evidence-ref)})
                   (catch Throwable _ nil)))
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
          path  ((requiring-resolve 'futon3c.aif.repl-trace/write-frame!) frame+ "data/repl-traces")
          ;; DOCUMENT stage: append the human-readable turn to the Pilot's Log (best-effort)
          cascade (let [c (first (filter #(= pre-top (:mission %)) (:cascade-policies post-j)))]
                    {:patterns (:shown c) :wholeness (:wholeness c)})
          logged (log-pilot-turn!
                  {:agent (:agent b) :mode (:mode b)
                   :recommendation {:target pre-top :g (get-in b [:pre :dT-snapshot 0 :g-total])}
                   :cascade cascade
                   :measurement {:predicted predicted :realised realised
                                 :top-shift? (not= pre-top post-top)
                                 :pre-top pre-top :post-top post-top}
                   :document document})
          ;; flight-as-derivation record (M-first-flights step 2): persist
          ;; the grounds this close already computed + whatever the pilot
          ;; supplied via :flight — missing pilot judgments become typed
          ;; sorries, never fabrications. Best-effort like the log.
          flight-path
          (try
            (let [rec ((requiring-resolve 'futon3c.aif.flight-record/compose-flight-record)
                       {:run-id run-id :begin b :agent (:agent b)
                        :predicted predicted
                        :predicted-constant (get-in b [:pre :predicted-constant])
                        :realised realised :realised-source realised-source
                        :executed? executed? :evidence-ref evidence-ref
                        :merge-event (when executed?
                                       {:type :operator-merge :at merge-at
                                        :note (str "executed close; evidence: " evidence-ref)})
                        :frame-path path :logged-turn (:turn logged)
                        :flight (cond-> (or flight {})
                                  (and (= :transient realised-read)
                                       (nil? (:class flight)))
                                  (assoc :class :transient))})]
              ((requiring-resolve 'futon3c.aif.flight-record/write-flight-record!)
               rec *live-runs-dir*))
            (catch Throwable _ nil))]
      (cond-> {:ok true :frame-path path :run-id run-id
               :predicted predicted :realised realised
               :prediction-error (Math/abs (double (- realised predicted)))
               :top-shift? (not= pre-top post-top) :pre-top pre-top :post-top post-top
               :logged-turn (:turn logged)
               :flight-path flight-path}
        executed? (assoc :independent? true :evidence-ref evidence-ref)))
    {:ok false :error (str "unknown run-id " run-id)})))
