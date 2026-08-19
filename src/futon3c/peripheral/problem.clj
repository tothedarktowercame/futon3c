(ns futon3c.peripheral.problem
  "Problem peripheral — one registered experimental problem per cycle."
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.cycle-harness :as apm-harness]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.evidence.backend :as evidence-backend]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.memory-lifecycle :as memory-lifecycle]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.peripheral.pull-receipts :as pull-receipts]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.substrate.client :as substrate])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]
           [java.util UUID]))

(def phase-order
  ;; :completed is a TERMINAL SENTINEL with no tools, matching the proof
  ;; peripheral's convention. The engine clears :current-phase and
  ;; :current-cycle-id the moment an advance returns the LAST phase, so the last
  ;; phase is a transition and never a state. With :close last, the machine
  ;; passed straight through it and :emit-trace / :validate-trace /
  ;; :write-authorization were unreachable -- which is why they were declared and
  ;; never implemented. Written-but-not-wired, because the PHASE was unreachable.
  [:register :frame :guided-solve :intervene :promote-solver :student-attempts
   :adjudicate :promote :close :completed])

(def advance :advance-problem-phase)

(def base-phase-tools
  {:register #{:read-registration :validate-registration :snapshot-store
               ;; :pin-resources was removed 2026-08-15. Since the engine began
               ;; stamping :environment-revision from the recorded
               ;; :assign-checkouts and :harness-revision from a measured
               ;; repository, a caller-invoked pin would be a SECOND and forgeable
               ;; route to fields the machine owns. The one rule this peripheral
               ;; implements is that the machine owns the fields it is answerable
               ;; for; a pin tool contradicts it.
               :freeze-stratum :assign-checkouts advance}
   :frame #{:emit-frame advance}
   :guided-solve #{:dispatch-solver :guide-solver :read-substrate advance}
   :intervene #{advance}
   :promote-solver #{:dispatch-scribe :promote-artifact :record-scribe-lanes advance}
   :student-attempts #{:dispatch-student-fresh :read-attempt-result advance}
   :adjudicate #{:write-disposition :write-use advance}
   ;; The scribe card mines a completed solve. :promote is the first phase
   ;; after adjudication devoted to turning that completed work into reusable
   ;; artifacts, so lane reports live beside promotion rather than solving.
   :promote #{:dispatch-scribe :promote-artifact :record-scribe-lanes advance}
   :close #{:record-measurement :emit-capability-probes :emit-trace :validate-trace
            :write-authorization advance}
   :completed #{}})

(def required-outputs
  {:register #{:registration :store-snapshot :stratum-frozen-at
               :environment-revision :harness-revision :environment-checkouts}
   :frame #{:frame :containment-probe}
   :guided-solve #{:solver-attempt :ground-control-events :memory-offers}
   :intervene #{:intervention}
   :promote-solver #{:promotion-result}
   :student-attempts #{:student-attempts :memory-uses}
   :adjudicate #{:disposition}
   :promote #{:promotion-result}
   :close #{:measurement :trace :validation :authorization}})

(def all-tools
  (into #{:begin-problem-cycle :load-registration :list-problems
          :problem-save :problem-load}
        (mapcat identity (vals base-phase-tools))))

(def tool-ops
  (zipmap all-tools
          (map #(if (#{:read-registration :validate-registration
                       :read-substrate :read-attempt-result :list-problems}
                     %)
                  :observe :action)
               all-tools)))

(defn- fruit [state]
  (some (fn [{:keys [tool result]}] (when (= :emit-trace tool) result))
        (reverse (:steps state))))

(defn- environment-arms-match [outputs]
  (let [registration (:registration outputs)
        ;; The registration's resource pins were decorative: nothing in src/,
        ;; test/ or scripts/ read :reg/environment-revision or
        ;; :reg/harness-revision, and :assign-checkouts takes a caller-supplied
        ;; :base-rev -- so a cycle provisioned at the WRONG revision validated
        ;; cleanly. The other checks compare the arms against EACH OTHER; this
        ;; one compares them against the frozen registration.
        pinned-env (:reg/environment-revision registration)
        pinned-harness (:reg/harness-revision registration)
        pinned (:environment-revision outputs)
        solver-revision (get-in outputs
                                [:solver-attempt :cycle/environment-revision])
        student-revisions (map :cycle/environment-revision
                               (:student-attempts outputs))
        solver-checkout (get-in outputs
                                [:solver-attempt :cycle/environment-checkout])
        student-checkouts (map :cycle/environment-checkout
                               (:student-attempts outputs))]
    (cond
      (and (some? pinned-env) (not= pinned-env pinned))
      {:failure :environment-revision-not-registered
       :registered pinned-env :provisioned pinned}

      (and (some? pinned-harness)
           (not (every? #(= pinned-harness %)
                        (keep :cycle/harness-revision
                              (cons (:solver-attempt outputs)
                                    (:student-attempts outputs))))))
      {:failure :harness-revision-not-registered
       :registered pinned-harness
       :actual (vec (keep :cycle/harness-revision
                          (cons (:solver-attempt outputs)
                                (:student-attempts outputs))))}

      (not (and (= pinned solver-revision)
                (every? #(= pinned %) student-revisions)))
      {:failure :environment-mismatch-between-arms
       :pinned pinned :solver solver-revision
       :students (vec student-revisions)}

      ;; A cold attempt has its own tree. Any nil or duplicate checkout leaves
      ;; an unmeasured filesystem channel between attempts.
      (let [checkouts (into [solver-checkout] student-checkouts)]
        (or (some nil? checkouts)
            (not= (count checkouts) (count (set checkouts)))))
      {:failure :environment-shared-checkout
       :solver solver-checkout :students (vec student-checkouts)})))

(defn- autoconf [context config]
  (let [intervention-tool (case (:cycle/mode context)
                            :store-mode :write-substrate
                            :harness-mode :tune-harness
                            nil)
        cycle-context (:cycle-context config)]
    (when cycle-context
      (reset! cycle-context context))
    (cond-> config
      intervention-tool
      (update-in [:phase-tools :intervene] conj intervention-tool))))

(defn- validate-loaded-state [current loaded]
  (when (not= (:cycle/mode current) (:cycle/mode loaded))
    {:failure :loaded-state-mode-mismatch
     :expected (:cycle/mode current)
     :actual (:cycle/mode loaded)}))

(defn- state-snapshot [state tool result]
  (when (and (= tool :problem-save) (:ok result))
    {:snapshot/subject {:ref/type :problem :ref/id "problem"}
     :snapshot/body {:snapshot :problem-save
                     :problem-id (:problem-id state)
                     :cycle-id (:current-cycle-id state)
                     :version (:version result)
                     :phase (:current-phase state)
                     :cycles-completed (:cycles-completed state)
                     :step-count (count (:steps state))}
     ;; make-snapshot-evidence already supplies :problem and :snapshot.
     ;; Repeating them here produces an invalid EvidenceEntry once the problem
     ;; peripheral's required evidence store actually exercises this path.
     :snapshot/tags []}))

(defn- stamp-attempt-environment [attempt assignment harness-revision]
  (if (map? attempt)
    (cond-> (assoc attempt
                   :cycle/environment-checkout (:checkout assignment)
                   :cycle/environment-revision (:base-revision assignment))
      (some? harness-revision)
      (assoc :cycle/harness-revision harness-revision))
    attempt))

(defn- recorded-harness-measurement [state]
  (some (fn [{:keys [tool result]}]
          (when (= tool :begin-problem-cycle)
            (select-keys result [:harness-revision :harness-tree-dirty?])))
        (reverse (:steps state))))

(defn- recorded-tool-result [state tool-id]
  (some (fn [{:keys [tool result]}]
          (when (= tool tool-id) result))
        (reverse (:steps state))))

(defn- recorded-cycle-tool-results
  "All results for `tool-id` since the current cycle's begin step, in order."
  [state tool-id]
  (->> (:steps state)
       reverse
       (take-while #(not= :begin-problem-cycle (:tool %)))
       reverse
       (keep (fn [{:keys [tool result]}]
               (when (= tool-id tool) result)))
       vec))

(defn- recorded-frame-output [state]
  (some-> (recorded-tool-result state :emit-frame)
          (select-keys [:frame :containment-probe])))

(defn- recorded-guide-events [state]
  (reduce (fn [events {:keys [tool result]}]
            (case tool
              :begin-problem-cycle []
              :guide-solver (conj events result)
              events))
          [] (:steps state)))

(defn- guidance-trace-events [events]
  (mapv #(select-keys % [:ground-control/recipient :ground-control/cycle
                         :ground-control/type :job-id])
        events))

(defn- recorded-solver-dispatches [state]
  (->> (:steps state)
       (keep (fn [{:keys [tool result]}]
               (when (and (= :dispatch-solver tool)
                          (:ground-control/solver-config result))
                 (select-keys result [:ground-control/recipient
                                      :ground-control/cycle
                                      :ground-control/solver-config
                                      :job-id]))))
       vec))

(defn- recorded-retrieval-probes [state]
  (->> (:steps state)
       reverse
       (take-while #(not= :begin-problem-cycle (:tool %)))
       reverse
       (keep (fn [{:keys [tool result]}]
               (when (#{:dispatch-solver :dispatch-student-fresh} tool)
                 (:retrieval-probe result))))
       vec))

(defn- recorded-pull-uses [state]
  (let [evidence-store (:evidence-store state)
        cycle-id (:current-cycle-id state)]
    (if-not evidence-store
      []
      (->> (:steps state)
           reverse
           (take-while #(not= :begin-problem-cycle (:tool %)))
           reverse
           (filter #(#{:dispatch-solver :dispatch-student-fresh} (:tool %)))
           (mapcat
            (fn [{:keys [result]}]
              (let [job-id (:job-id result)]
                (when job-id
                  (for [receipt (pull-receipts/pull-use-receipts
                                 evidence-store job-id)
                        :let [body (:evidence/body receipt)]]
                    {:pull/memory-id (:memory-id body)
                     :pull/seat (:agent-id body)
                     :pull/tool (:tool body)
                     :pull/cycle cycle-id
                     :pull/job-id job-id
                     :pull/at (:at body)})))))
           vec))))

(def scribe-lanes
  ;; Source: holes/labs/M-apm-demonstration/role-cards/scribe.md, "Four lanes".
  #{:solve :arc :trajectory :challenge})

(defn- record-scribe-lanes-from-state [state args]
  (let [report (first args)
        lane (:lane report)
        ran? (:ran? report)
        yield (:yield report)
        author (:author report)
        staffed-seat (get-in state
                              [:cycle/outputs :registration :reg/scribe-seat])]
    (when-not (contains? scribe-lanes lane)
      (throw (ex-info ":unknown-scribe-lane — unknown scribe lane"
                      {:failure :unknown-scribe-lane :lane lane})))
    (when-not (boolean? ran?)
      (throw (ex-info ":malformed-scribe-lane-report — ran? must be boolean"
                      {:failure :malformed-scribe-lane-report
                       :field :ran?})))
    (when-not (and (vector? yield) (every? prereg/nonblank-string? yield))
      (throw (ex-info ":malformed-scribe-lane-report — yield must be a vector of memory ids"
                      {:failure :malformed-scribe-lane-report
                       :field :yield})))
    (when-not (prereg/nonblank-string? author)
      (throw (ex-info ":malformed-scribe-lane-report — author must be a staffed seat name"
                      {:failure :malformed-scribe-lane-report
                       :field :author})))
    (when (and (prereg/nonblank-string? staffed-seat)
               (not= staffed-seat author))
      (throw (ex-info ":scribe-seat-mismatch — author does not match registered seat"
                      {:failure :scribe-seat-mismatch
                       :registered staffed-seat
                       :author author})))
    (select-keys report [:lane :ran? :yield :author])))

(defn- recorded-student-assignments [state]
  ;; :steps spans the whole peripheral session, including completed cycles.
  ;; Reset at the latest assignment so attempt 1 of a new cycle cannot be
  ;; paired with attempt 1's tree from the previous cycle.
  (reduce (fn [assignments {:keys [tool result]}]
            (case tool
              :assign-checkouts []
              :dispatch-student-fresh
              (conj assignments (:environment-checkout result))
              assignments))
          [] (:steps state)))

(defn- recorded-assignment
  "What :assign-checkouts ACTUALLY produced, from the engine's own step record.

  Preferred over the payload because :environment-checkouts otherwise arrives
  through the caller-supplied advance payload like every other output -- so a
  caller could relay a fabricated assignment and everything downstream would
  stamp consistently from the fabrication, with every check passing against
  paths that were never provisioned. The engine writes tool results into :steps
  and the caller cannot edit them. Same shape as `fruit`, which reads the
  :emit-trace result from step history."
  [state payload]
  (or (some (fn [{:keys [tool result]}]
              (when (= :assign-checkouts tool)
                (:environment-checkouts result)))
            (reverse (:steps state)))
      (get-in state [:cycle/outputs :environment-checkouts])
      (:environment-checkouts payload)))

(defn- student-memory-eligibility
  "Machine-owned eligibility for the student pull channel.

  Harness mode remains frozen at the open snapshot. Store mode additionally
  admits artifacts promoted by recorded tool results earlier in this cycle.
  Both source vectors are retained so the receipt can audit the union."
  [state]
  (let [snapshot-ids (vec (or (get-in state
                                      [:cycle/outputs :store-snapshot
                                       :snap/memory-ids]) []))
        promoted-ids (->> (get-in state [:cycle/outputs :promotion-result])
                          (keep :promo/artifact-id)
                          distinct
                          vec)
        store-mode? (= :store-mode (:cycle/mode state))
        eligible-ids (->> (if store-mode?
                            (concat snapshot-ids promoted-ids)
                            snapshot-ids)
                          distinct
                          vec)]
    {:eligible-memory-ids eligible-ids
     :eligible-memory-provenance
     {:policy (if store-mode?
                :snapshot-union-cycle-promoted
                :snapshot-only)
      :snapshot-memory-ids snapshot-ids
      :cycle-promoted-memory-ids (if store-mode? promoted-ids [])}}))

(declare recorded-measurement recorded-close-envelope recorded-validation
         recorded-authorization)

(defn- stamp-environment-outputs [state payload]
  (let [closing? (= :close (:current-phase state))
        payload (if closing?
                  ;; Close artifacts are retained derived-step results, never
                  ;; advance-payload assertions from the caller.
                  ;; Retrieval probes are also machine-owned: retained dispatch
                  ;; results replace this key below. An injected empty vector must
                  ;; never erase a real probe or impersonate its producer.
                  (apply dissoc payload
                         [:measurement :trace :validation :authorization
                          :retrieval-probes])
                  payload)
        measurement (recorded-measurement state)
        close-envelope (recorded-close-envelope state)
        validation (recorded-validation state)
        authorization (recorded-authorization state)
        assignments (recorded-assignment state payload)
        solver (:solver assignments)
        students (let [recorded (recorded-student-assignments state)]
                   (if (seq recorded) recorded (vec (:student assignments))))
        revision (:base-revision solver)
        harness (recorded-harness-measurement state)
        harness-revision (:harness-revision harness)
        store-snapshot (recorded-tool-result state :snapshot-store)
        frame-output (recorded-frame-output state)
        dispositions (recorded-cycle-tool-results state :write-disposition)
        memory-uses (recorded-cycle-tool-results state :write-use)
        pull-uses (recorded-pull-uses state)
        promotions (recorded-cycle-tool-results state :promote-artifact)
        scribe-lane-reports
        (recorded-cycle-tool-results state :record-scribe-lanes)
        retrieval-probes (recorded-retrieval-probes state)
        frozen-at (:cycle/stratum-frozen-at
                   (recorded-tool-result state :freeze-stratum))
        assigned-at (:assigned-at (recorded-tool-result state :assign-checkouts))]
    (cond-> payload
      (some? measurement)
      (assoc :measurement measurement)

      (some? (:trace close-envelope))
      (assoc :trace (:trace close-envelope))

      (some? validation)
      (assoc :validation validation)

      (some? authorization)
      (assoc :authorization authorization)

      ;; The recorded assignment overwrites the caller's, exactly as the attempt
      ;; fields do -- the caller may relay it, but the machine owns it.
      (and (some? assignments) (contains? payload :environment-checkouts))
      (assoc :environment-checkouts assignments)

      (some? revision)
      (assoc :environment-revision revision)

      (some? harness-revision)
      (assoc :harness-revision harness-revision
             :harness-tree-dirty? (:harness-tree-dirty? harness))

      (some? store-snapshot)
      (assoc :store-snapshot store-snapshot)

      (seq frame-output)
      (merge frame-output)

      (seq dispositions)
      (assoc :disposition dispositions)

      (seq memory-uses)
      (assoc :memory-uses memory-uses)

      (seq pull-uses)
      (assoc :pull-uses pull-uses)

      (seq promotions)
      (assoc :promotion-result promotions)

      (seq scribe-lane-reports)
      (assoc :scribe-lane-reports scribe-lane-reports)

      (seq retrieval-probes)
      (assoc :retrieval-probes retrieval-probes)

      (some? frozen-at)
      (assoc :stratum-frozen-at frozen-at)

      (some? assigned-at)
      (assoc :assigned-at assigned-at)

      (= :guided-solve (:current-phase state))
      (assoc :ground-control-events (recorded-guide-events state)
             :solver-dispatches (recorded-solver-dispatches state))

      (contains? payload :solver-attempt)
      (update :solver-attempt stamp-attempt-environment solver harness-revision)

      (sequential? (:student-attempts payload))
      (update :student-attempts
              #(mapv (fn [index attempt]
                       (stamp-attempt-environment attempt (get students index)
                                                  harness-revision))
                     (range) %)))))

(defn- harness-tree-clean [outputs]
  (when (:harness-tree-dirty? outputs)
    {:failure :harness-tree-dirty}))

(defn- seat-registration-valid [outputs]
  ;; Full shape validation remains observable through :validate-registration.
  ;; This invariant makes the card/seat findings introduced here hard gates
  ;; without retroactively turning every legacy partial registration used by
  ;; the peripheral into a new refusal surface.
  (let [failures (filter #(or (= :guide-proctor-not-separated %)
                              (= :unstaffed-carded-seat (:finding %)))
                         (prereg/registration-shape-failures
                          (:registration outputs)))]
    (when (seq failures)
      {:failure :registration-shape-invalid
       :findings (vec failures)})))

(defn- thread-current-phase [state tool args]
  (case tool
    :advance-problem-phase
    (conj (vec args) {:cycle/current-phase (:current-phase state)})

    :emit-frame
    (conj (vec args) {:cycle/id (:current-cycle-id state)})

    :guide-solver
    (conj (vec args)
          {:cycle/id (:current-cycle-id state)
           :solver-seat (get-in state
                                [:cycle/outputs :registration :reg/solver-seat])
           :guidance-regime
           (get-in state
                   [:cycle/outputs :registration :reg/guidance-regime])})

    :dispatch-solver
    (conj (vec args) {:cycle/id (:current-cycle-id state)
                      :cycle/step-index (count (:steps state))
                      :solver-config
                      (get-in state
                              [:cycle/outputs :registration :reg/solver-config])})

    :dispatch-student-fresh
    (conj (vec args) (merge {:cycle/id (:current-cycle-id state)
                             :cycle/step-index (count (:steps state))
                             :student-runner-budget
                             (or (get-in state
                                         [:cycle/outputs :registration
                                          :reg/student-runner-budget])
                                 {:wall-clock-minutes 60})}
                            (student-memory-eligibility state)))

    :dispatch-scribe
    (conj (vec args) {:cycle/id (:current-cycle-id state)
                      :cycle/step-index (count (:steps state))
                      :scribe-seat
                      (get-in state
                              [:cycle/outputs :registration :reg/scribe-seat])})

    :read-attempt-result
    (conj (vec args) {:cycle/outputs (:cycle/outputs state)})

    ;; F4 asks whether the stratum was frozen BEFORE the checkouts were
    ;; assigned. That is an ordering of events, so the value must be a LOGICAL
    ;; clock, not a wall or monotonic one. System/nanoTime was the first
    ;; implementation and its origin is arbitrary and per-JVM: comparable only
    ;; within one process. Save/load across a restart is a designed feature of
    ;; this peripheral, and the round spans two machines, so a nanoTime pair
    ;; recorded either side of a restart is meaningless -- F4 would refuse a
    ;; sound cycle or pass a broken one. The engine's own step index is
    ;; persisted in :steps, survives save/load, and is identical on any host.
    (:freeze-stratum :assign-checkouts :write-disposition :write-use
     :promote-artifact)
    (conj (vec args) {:cycle/step-index (count (:steps state))})

    args))

(defn- now-string []
  (str (Instant/now)))

(defn- output-entities [outputs key]
  (let [value (get outputs key)]
    (cond
      (nil? value) []
      (and (sequential? value) (not (map? value))) (vec value)
      :else [value])))

(defn- cycle-opened-at [state]
  (or (:cycle/opened-at state)
      (some (fn [{:keys [tool result]}]
              (when (= tool :begin-problem-cycle)
                (:cycle/opened-at result)))
            (reverse (:steps state)))))

(def ^:private capability-attesting-tools
  ;; These pairings are deliberately narrower than "a nearby phase".
  ;; :need-retrieval was the last capability with no step exercising it, and it
  ;; got no synthetic probe -- correctly, while nothing produced a retrieval
  ;; probe. :dispatch-solver now emits one (:rprobe with pre-cutoff available ids
  ;; and post-cutoff retrieved ids), so the capability has a real attesting step
  ;; and no longer has to report itself missing.
  {:need-retrieval :dispatch-solver
   :created-frame-worked :emit-frame
   :frame-containment-witnessed :emit-frame
   :unique-disposition :write-disposition
   :offer-use-disposition :write-use
   :promotion-importable :promote-artifact
   :promotion-need-taggable :promote-artifact
   :measurement-populated :record-measurement})

(defn- latest-step [state tool]
  (some #(when (= tool (:tool %)) %) (reverse (:steps state))))

(defn- measurement-values [outputs]
  (let [dispositions (output-entities outputs :disposition)
        disposition (when (= 1 (count dispositions)) (first dispositions))
        scribe-reports (:scribe-lane-reports outputs)
        ran-lanes (when (seq scribe-reports)
                    (->> scribe-reports
                         (filter :ran?)
                         (map :lane)
                         distinct
                         sort
                         vec))
        arc-yield (when (seq scribe-reports)
                    (->> scribe-reports
                         (filter #(and (= :arc (:lane %)) (:ran? %)))
                         (mapcat :yield)
                         count))
        promoted-memory-ids (->> (:promotion-result outputs)
                                 (filter #(and (contains? % :promo/artifact-id)
                                               (contains? % :promo/pattern-id)))
                                 (mapv :promo/artifact-id))
        locked-exposure (get-in outputs
                                [:registration :problem
                                 :locked-lemma-exposure])]
    (cond-> {}
      (contains? disposition :disp/outcome)
      (assoc "terminal disposition" (:disp/outcome disposition))

      (contains? disposition :disp/residual-sorries)
      (assoc "residual executable sorries"
             (:disp/residual-sorries disposition))

      (contains? disposition :disp/axiom-clean?)
      (assoc "axiom cleanliness" (:disp/axiom-clean? disposition))

      (vector? locked-exposure)
      (assoc "locked-lemma exposure" locked-exposure)

      (sequential? (:ground-control-events outputs))
      (assoc "attempts or closer hops"
             (count (:ground-control-events outputs)))

      (contains? outputs :promotion-result)
      (assoc "memories promoted"
             {:count (count promoted-memory-ids)
              :ids promoted-memory-ids})

      (seq scribe-reports)
      (assoc "scribe lane coverage"
             {:lanes-ran ran-lanes
              :ran (count ran-lanes)
              :total (count scribe-lanes)}
             "arc-lane yield" arc-yield))))

(defn- unset-measurement-reason [field]
  (case field
    "statement defects at review"
    "unset: no formalizer review record is present in cycle outputs"
    "attempts or closer hops"
    "unset: Agency-derived guidance evidence is not available to this tool"
    "memories promoted"
    "unset: no promotion-result is present in cycle outputs"
    "review escape rate"
    "unset: requires a join of review and later gate histories"
    "promoted then surfaced then used"
    "unset: requires a cross-cycle promotion, offer, and use join"
    "contract leaks"
    "unset: no def-body freeze hash is recorded"
    "duplicate declarations"
    "unset: requires a corpus-wide declaration scan"
    "promotion coverage"
    "unset: requires the corpus-wide proved-helper denominator"
    "unconsumed promotions"
    "unset: requires downstream consumer evidence"
    "import-only edges"
    "unset: requires import and declaration-use analysis"
    "scribe lane coverage"
    "unset: no scribe-lane event is present in cycle outputs"
    "arc-lane yield"
    "unset: no arc-lane event is present in cycle outputs"
    "rewrite rule offered and used"
    "unset: no typed rewrite-rule offer/use join is present"
    "unset: no matching machine-derived source is present in cycle outputs"))

(defn- measurement-unset-reasons [missing-fields]
  (into {} (map (juxt identity unset-measurement-reason)) missing-fields))

(defn- record-measurement-from-state [state _args]
  (let [outputs (:cycle/outputs state)
        required (get-in outputs [:registration :required-measurement-fields])
        values (measurement-values outputs)
        unset (measurement-unset-reasons (remove #(contains? values %) required))
        covered (into (set (keys values)) (keys unset))
        missing (remove covered required)]
    (when (seq missing)
      (throw (ex-info (str "record-measurement silently omitted fields: "
                           (str/join ", " missing))
                      {:failure :measurement-field-silently-omitted
                       :fields (vec missing)})))
    (when-let [blank-reason (some (fn [[field reason]]
                                   (when (str/blank? reason) field))
                                 unset)]
      (throw (ex-info "record-measurement produced a blank unset reason"
                      {:failure :measurement-unset-reason-blank
                       :field blank-reason})))
    {:meas/id (str "meas/" (:current-cycle-id state))
     :meas/values values
     :meas/unset unset}))

(defn- recorded-measurement [state]
  (:result (latest-step state :record-measurement)))

(defn- recorded-close-envelope [state]
  (:result (latest-step state :emit-trace)))

(defn- recorded-validation [state]
  (:result (latest-step state :validate-trace)))

(defn- recorded-authorization [state]
  (:result (latest-step state :write-authorization)))

(defn- emit-capability-probes-from-state [state _args]
  (->> prereg/required-capabilities
       (keep (fn [capability]
               (when-let [tool (get capability-attesting-tools capability)]
                 (when-let [evidence-id (:evidence/id (latest-step state tool))]
                   {:probe/id (str "probe/" (:current-cycle-id state) "/"
                                   (name capability))
                    :probe/capability capability
                    :probe/evidence-id evidence-id
                    :probe/recorded? true}))))
       vec))

(defn- recorded-capability-probes [state]
  (some (fn [{:keys [tool result]}]
          (when (= tool :emit-capability-probes) result))
        (reverse (:steps state))))

(defn- emit-trace-from-state
  "Project the engine-owned cycle record through the existing APM projection.

  Producer presence is distinct from cardinality: an output key with an empty
  collection proves its producer ran and is passed to the validator; a missing
  key means there is no evidence that the producer ran and fails here."
  [state _args]
  (let [recorded-probes (recorded-capability-probes state)
        measurement (recorded-measurement state)
        ;; Capability and retrieval probes are never accepted from the advance
        ;; payload. Only
        ;; the engine-derived tool's retained step result can supply them.
        ;; Retrieval probes are reconstructed from retained dispatch results;
        ;; caller-supplied probes are discarded even when no dispatch produced one.
        recorded-retrieval-probes (recorded-retrieval-probes state)
        outputs (cond-> (dissoc (:cycle/outputs state)
                                :capability-probes :measurement :retrieval-probes)
                  (some? recorded-probes)
                  (assoc :capability-probes recorded-probes)
                  (some? measurement)
                  (assoc :measurement measurement)
                  (seq recorded-retrieval-probes)
                  (assoc :retrieval-probes recorded-retrieval-probes))
        producer-keys {:registration :registration
                       :frame :frame
                       :store-snapshot :store-snapshot
                       :containment-probe :containment-probe
                       :measurement :measurement
                       :solver-attempt :solver-attempt
                       :student-attempt :student-attempts
                       :disposition :disposition
                       :memory-offer :memory-offers
                       :memory-use :memory-uses
                       :retrieval-probe :retrieval-probes
                       :capability-probe :capability-probes
                       :promotion :promotion-result}
        missing (->> producer-keys
                     (keep (fn [[entity-type output-key]]
                             (when-not (contains? outputs output-key)
                               entity-type)))
                     vec)
        producer-failures
        (mapv (fn [entity-type]
                {:failure :missing-trace-entity-producer
                 :entity-type entity-type})
              missing)
        cycle-id (:current-cycle-id state)
          cycle-entity {:cycle/id cycle-id
                        :cycle/opened-at (cycle-opened-at state)
                        ;; Emission is the closing record. The machine reads the
                        ;; clock; no timestamp is accepted from tool arguments.
                        :cycle/closed-at (now-string)
                        :cycle/mode (:cycle/mode state)
                        :cycle/deposit-state (:cycle/deposit-state state)
                        :cycle/paired-with (:cycle/paired-with state)
                        :cycle/stratum-frozen-at (:stratum-frozen-at outputs)
                        :cycle/assigned-at (:assigned-at outputs)}
          ;; derive-trace locates entities by LINKING KEYS -- :frame/cycle,
          ;; :attempt/cycle, :offer/cycle and so on -- and phase outputs do not
          ;; carry them. Passing outputs through unstamped made `one` throw
          ;; "expected exactly one entity" on every real cycle; the unit tests
          ;; missed it because they stub derive-trace, so the projection they
          ;; assert is called is never actually run.
          link (fn [k entities] (mapv #(assoc % k cycle-id) entities))
          frames (link :frame/cycle (output-entities outputs :frame))
          frame-id (:frame/id (first frames))
          attempts (map-indexed
                    (fn [i attempt]
                      (assoc attempt :attempt/cycle cycle-id :attempt/seq i))
                    (concat (output-entities outputs :solver-attempt)
                            (output-entities outputs :student-attempts)))
          entities (vec
                    (concat
                     [cycle-entity]
                     frames
                     (link :snap/cycle (output-entities outputs :store-snapshot))
                     (mapv #(assoc % :cprobe/frame frame-id)
                           (output-entities outputs :containment-probe))
                     (link :meas/cycle (output-entities outputs :measurement))
                     attempts
                     (link :disp/cycle (output-entities outputs :disposition))
                     (link :offer/cycle (output-entities outputs :memory-offers))
                     (output-entities outputs :memory-uses)
                     (output-entities outputs :pull-uses)
                     (link :rprobe/cycle (output-entities outputs :retrieval-probes))
                     (link :probe/cycle (output-entities outputs :capability-probes))
                     (link :promo/cycle (output-entities outputs :promotion-result))))
          projection (try
                       {:trace (apm-harness/derive-trace
                                (:registration outputs) cycle-id entities)}
                       (catch Throwable t
                         {:projection-failure
                          {:failure :trace-projection-failed
                           :message (.getMessage t)}}))
          trace (some-> (:trace projection)
                        (assoc :guidance-events
                               (guidance-trace-events
                                (:ground-control-events outputs))
                               :action-refusals
                               (vec (:cycle/action-refusals state))
                               :measurement-summary
                               {:measured (count (get-in outputs
                                                        [:measurement :meas/values]))
                                :unset (count (get-in outputs
                                                     [:measurement :meas/unset]))}))
          trace (cond-> trace
                  (seq (:solver-dispatches outputs))
                  (assoc :solver-dispatches
                         (vec (:solver-dispatches outputs))))]
      {:trace trace
     :producer-failures (cond-> producer-failures
                          (:projection-failure projection)
                          (conj (:projection-failure projection)))}))

(defn- validate-trace-from-state [state _args]
  (let [{:keys [trace producer-failures]} (recorded-close-envelope state)
        registration (get-in state [:cycle/outputs :registration])
        report (prereg/report registration trace (:lean-repo state)
                              (:agency-endpoint state)
                              (:reg/solver-seat registration))
        failures (vec (concat producer-failures (:failures report)))]
    {:trace trace
     :producer-failures (vec producer-failures)
     :validation-report report
     :failures failures
     :launchable? (and (empty? producer-failures) (:launchable? report))}))

(defn- write-authorization-from-state [state _args]
  (let [{:keys [trace validation-report failures launchable?] :as validation}
        (recorded-validation state)
        output (:authorization-output state)
        revision (:authorization-revision state)
        at (now-string)]
    (if-not launchable?
      {:written? false :refused? true :at at :failures (vec failures)
       :validation validation}
      (do
        (when-not (and (string? output) (not (str/blank? output)))
          (throw (ex-info "authorization output is absent from cycle context"
                          {:failure :authorization-output-unavailable})))
        (when-not (and (string? revision)
                       (re-matches #"[0-9a-f]{40}" revision))
          (throw (ex-info "authorization revision is absent or malformed"
                          {:failure :authorization-revision-unavailable})))
        (let [registration (get-in state [:cycle/outputs :registration])
              authorization
              {:kind :apm-demonstration-round1-launch-authorization
               :schema 1
               :authorization-revision revision
               :lean-revision (:lean-revision registration)
               :registration-sha256
               (apm-harness/sha256-bytes (.getBytes (pr-str registration) "UTF-8"))
               :trace-sha256
               (apm-harness/sha256-bytes (.getBytes (pr-str trace) "UTF-8"))
               :problem (:problem registration)
               :validation validation-report}]
          (io/make-parents output)
          (spit output (str (pr-str authorization) "\n"))
          {:written? true :refused? false :at at :output output
           :authorization authorization})))))

(def problem-domain-config
  {:domain-id :problem
   :phase-order phase-order
   :phase-tools base-phase-tools
   :setup-tools #{:begin-problem-cycle :load-registration :list-problems}
   :tool-ops (assoc tool-ops
                    :write-substrate :action
                    :tune-harness :action)
   :required-outputs required-outputs
   :enforce-required-outputs? true
   :state-io-tools {:save :problem-save :load :problem-load}
   :always-available-tools #{:problem-save :problem-load}
   :state-runtime-keys #{:cycle-config :evidence-store}
   :state-validate-fn validate-loaded-state
   :state-snapshot-fn state-snapshot
   :backend-args-fn thread-current-phase
   :output-stamp-fn stamp-environment-outputs
   :derived-tools {:record-measurement record-measurement-from-state
                   :record-scribe-lanes record-scribe-lanes-from-state
                   :emit-capability-probes emit-capability-probes-from-state
                   :emit-trace emit-trace-from-state
                   :validate-trace validate-trace-from-state
                   :write-authorization write-authorization-from-state}
   :output-invariants
   [{:id :seat-registration-valid
     :requires #{:registration}
     :check seat-registration-valid}
    {:id :harness-tree-clean
     :requires #{:harness-tree-dirty?}
     :check harness-tree-clean}
    {:id :environment-arms-match
     :requires #{:registration :environment-revision :solver-attempt
                :student-attempts}
     :check environment-arms-match}]
   :cycle-begin-tool :begin-problem-cycle
   :cycle-advance-tool advance
   :state-init-fn (fn [context]
                    {:problem-id (:problem-id context)
                     :cycle/mode (:cycle/mode context)
                     :cycle/deposit-state (:cycle/deposit-state context)
                     :cycle/paired-with (:cycle/paired-with context)
                     :lean-repo (:lean-repo context)
                     :harness-repo (:harness-repo context)
                     :agency-endpoint (:agency-endpoint context)
                     :authorization-revision (:authorization-revision context)
                     :authorization-output (:authorization-output context)})
   :autoconf-fn autoconf
   :fruit-fn fruit
   :exit-context-fn (fn [state]
                      {:session-id (:session-id state)
                       :problem-id (:problem-id state)})})

(def problem-spec
  {:peripheral/id :problem
   :peripheral/tools (conj all-tools :write-substrate :tune-harness)
   :peripheral/scope :full-codebase
   :peripheral/entry #{:user-request}
   :peripheral/exit #{:user-request}
   :peripheral/context {}})

(def ^:private dispatch-channels
  {:dispatch-solver :push+pull
   :dispatch-student-fresh :pull-only
   :dispatch-scribe :pull-only})

(def ^:private default-problem-state-root "data/problem-state")
(def ^:private problem-state-write-lock (Object.))
(def ^:private frames-script "/home/joe/code/futon3c/scripts/frames.bb")
(def ^:private experiment-frames-root
  "/home/joe/code/futon3c/data/experiment-frames")

(defn- cycle-dir-within
  "Resolve the per-cycle directory and REQUIRE it to sit under root.

  The character-class check alone is not path safety: \"..\" matches
  [A-Za-z0-9._-]+ because both dots are in the class, and a save with that cycle
  id wrote to <root>/../v1.edn -- outside the store. A regex that looks like a
  containment check is worse than none, because it stops anyone looking. So the
  resolved canonical path is compared against the resolved canonical root."
  [root cycle-id]
  (when-not (and (string? cycle-id)
                 (re-matches #"[A-Za-z0-9._-]+" cycle-id)
                 (not (contains? #{"." ".."} cycle-id)))
    (throw (ex-info "Problem state requires a safe cycle id"
                    {:cycle-id cycle-id})))
  (let [root-path (.toAbsolutePath (.normalize (.toPath (io/file root))))
        dir (io/file root cycle-id)
        dir-path (.toAbsolutePath (.normalize (.toPath dir)))]
    (when-not (.startsWith dir-path root-path)
      (throw (ex-info "Problem state path escapes its root"
                      {:cycle-id cycle-id :resolved (str dir-path)})))
    dir))

(defn- safe-cycle-id [state]
  (let [cycle-id (:current-cycle-id state)]
    (when-not (and (string? cycle-id)
                   (re-matches #"[A-Za-z0-9._-]+" cycle-id)
                   (not (contains? #{"." ".."} cycle-id)))
      (throw (ex-info "Problem state requires a safe active cycle id"
                      {:current-cycle-id cycle-id})))
    cycle-id))

(defn- version-number [filename]
  (some->> (re-matches #"v([0-9]+)\.edn" filename)
           second
           parse-long))

(defn- existing-versions [cycle-dir]
  (if (.isDirectory cycle-dir)
    (->> (.listFiles cycle-dir)
         (keep #(version-number (.getName %)))
         sort)
    []))

(defn- save-problem-state! [root state]
  ;; Version selection and rename are one JVM-critical section. ATOMIC_MOVE
  ;; makes publication indivisible, but Java leaves replacement of an existing
  ;; target implementation-specific; serial allocation prevents two engine
  ;; saves from ever presenting it with the same target.
  (locking problem-state-write-lock
    (let [cycle-id (safe-cycle-id state)
          cycle-dir (cycle-dir-within root cycle-id)
          _ (Files/createDirectories (.toPath cycle-dir)
                                     (make-array FileAttribute 0))
          version (inc (or (last (existing-versions cycle-dir)) 0))
          target (.toPath (io/file cycle-dir (str "v" version ".edn")))
          temp (Files/createTempFile (.toPath cycle-dir) ".state-" ".tmp"
                                     (make-array FileAttribute 0))]
      (try
        (spit (.toFile temp) (pr-str state))
        (Files/move temp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE]))
        {:ok true :version version :path (str target)}
        (finally
          (Files/deleteIfExists temp))))))

(defn- load-problem-state [root cycle-id version]
  (when-not (pos-int? version)
    (throw (ex-info "Problem state version must be a positive integer"
                    {:version version})))
  (edn/read-string (slurp (io/file (cycle-dir-within root cycle-id)
                                   (str "v" version ".edn")))))

(defrecord ProblemStateBackend [inner-backend root]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (case tool-id
      ;; ToolBackend promises {:ok true :result} | {:ok false :error}. A missing
      ;; version made slurp throw FileNotFoundException straight out of
      ;; execute-tool, so the cycle could not record its own failure -- the third
      ;; instance of this same boundary defect in this session.
      :problem-save
      (let [[state] args]
        (try {:ok true :result (save-problem-state! root state)}
             (catch Throwable t
               {:ok false :error (str "problem-save failed: " (.getMessage t))})))

      :problem-load
      (let [[cycle-id version] args]
        (try {:ok true :result (load-problem-state root cycle-id version)}
             (catch Throwable t
               {:ok false :error (str "problem-load failed: " (.getMessage t))})))

      (tools/execute-tool inner-backend tool-id args))))

(defn make-problem-state-backend [inner-backend root]
  (->ProblemStateBackend inner-backend root))

(defn- require-provision-option [options key]
  (let [value (get options key)]
    (when-not (and (string? value) (not (str/blank? value)))
      (throw (ex-info (str "assign-checkouts requires " key) {:key key})))
    value))

(defn- provision-frame! [options]
  (let [problem (require-provision-option options :problem)
        arm (require-provision-option options :arm)
        batch (require-provision-option options :batch)
        base-rev (require-provision-option options :base-rev)
        seat (require-provision-option options :seat)
        memory-channel (require-provision-option options :memory-channel)
        recall-system (require-provision-option options :recall-system)
        branch (require-provision-option options :branch)
        frame-id (str batch "-" problem "-" arm)
        command ["bb" frames-script "open"
                 "--problem" problem "--arm" arm
                 "--base-rev" base-rev "--seat" seat
                 "--memory-channel" memory-channel
                 "--recall-system" recall-system
                 "--batch" batch "--branch" branch]
        {:keys [exit out err]} (apply shell/sh command)]
    (when-not (zero? exit)
      (let [details (->> [err out]
                         (remove str/blank?)
                         (str/join "\n"))]
        (throw (ex-info (str "frames.bb open failed (exit " exit ")"
                             (when-not (str/blank? details)
                               (str ":\n" details)))
                        {:exit exit :stdout out :stderr err
                         :frame-id frame-id}))))
    (let [record (edn/read-string
                  (slurp (io/file experiment-frames-root batch
                                  (str frame-id ".edn"))))]
      (select-keys record [:checkout :base-revision :branch :frame/id :batch]))))

(defn- checkout-options [options arm]
  (let [arm-name (name arm)
        batch (require-provision-option options :batch)
        problem (require-provision-option options :problem)]
    {:problem problem
     :arm arm-name
     :batch batch
     :base-rev (require-provision-option options :base-rev)
     :seat (require-provision-option options
                                    (if (= arm :solver)
                                      :solver-seat
                                      :student-seat))
     :memory-channel (if (= arm :solver) "push" "none")
     :recall-system (require-provision-option options :recall-system)
     ;; The script's default omits batch and collides on the second cycle.
     :branch (str "exp/" batch "-" problem "-" arm-name)}))

(def ^:private apm-root "/home/joe/code/apm-lean")

(defn- rollback-frame!
  "Undo one provisioned frame: worktree, branch, and record.

  Assignment must be ALL-OR-NOTHING. Verified before this existed: when the
  student arm failed, the solver's worktree and branch survived, and the retry
  then died on \"frame already exists\" -- a half-provisioned cycle that could not
  be re-registered without manual cleanup. A sticky failure is worse than a loud
  one, because the obvious response (try again) cannot work."
  [{:keys [checkout branch] :as frame}]
  (when checkout
    (shell/sh "git" "-C" apm-root "worktree" "remove" "--force" checkout))
  (when branch
    (shell/sh "git" "-C" apm-root "branch" "-D" branch))
  (when-let [frame-id (:frame/id frame)]
    (let [batch (or (:batch frame) (first (str/split frame-id #"-")))]
      (io/delete-file (io/file experiment-frames-root batch
                               (str frame-id ".edn")) true)))
  (shell/sh "git" "-C" apm-root "worktree" "prune"))

(defrecord CheckoutProvisioningBackend
    [inner-backend provisioner-fn rollback-fn assignment-context
     provisioned-frames]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (cond
      (= tool-id :assign-checkouts)
      (let [[options] args
            done (atom [])]
        (try
          (let [solver (provisioner-fn (checkout-options options :solver))]
            (swap! done conj solver)
            (reset! assignment-context options)
            (reset! provisioned-frames [solver])
            {:ok true
             :result {:environment-checkouts {:solver solver :student []}}})
          (catch Throwable t
            ;; Roll back whatever already succeeded, so a retry is possible.
            (doseq [frame @done]
              (try (rollback-fn frame) (catch Throwable _ nil)))
            {:ok false
             :error (str "assign-checkouts failed: " (.getMessage t))
             :rolled-back (count @done)})))

      (= tool-id :dispatch-student-fresh)
      (let [[opts packet] (take 2 args)
            measured (last args)
            context @assignment-context]
        (if-not context
          {:ok false :error "student dispatch has no recorded checkout assignment"}
          (let [arm (str "student-" (UUID/randomUUID))
                frame (try (provisioner-fn (checkout-options context arm))
                           (catch Throwable t t))]
            (if (instance? Throwable frame)
              ;; Roll back NOTHING else. All-or-nothing is correct at
              ;; :assign-checkouts, where no work has been done yet; here the
              ;; solver may have spent the whole cycle in its tree. Rolling the
              ;; assignment back on a student provisioning failure DELETED THE
              ;; SOLVER'S WORKTREE AND BRANCH -- verified: a transient failure on
              ;; student attempt 2 rolled back ["/tree/solver" "/tree/student-1"].
              ;; That inverts the reason all-or-nothing exists: it was so a retry
              ;; would be possible, and this made retry mean redoing the solve.
              ;; frames.bb removes its own partial worktree internally, so a
              ;; failed provision leaves nothing behind to undo.
              {:ok false
               :error (str "student checkout provisioning failed: "
                           (.getMessage ^Throwable frame))
               :rolled-back 0}
              (let [packet (str packet
                                "\n--- ENVIRONMENT (provisioned by dispatcher) ---\n"
                                "Your checkout: " (:checkout frame) "\n"
                                "Branch: " (:branch frame) "\n"
                                "Base revision: " (:base-revision frame) "\n"
                                "Work ONLY in this checkout; commit ONLY to this branch.\n")
                    dispatch (tools/execute-tool
                              inner-backend tool-id
                              [(assoc (or opts {})
                                      :environment-checkout (:checkout frame)
                                      :environment-revision (:base-revision frame))
                               packet measured])]
                (if (:ok dispatch)
                  (do
                    (swap! provisioned-frames conj frame)
                    (assoc-in dispatch [:result :environment-checkout] frame))
                  (do
                    (try (rollback-fn frame) (catch Throwable _ nil))
                    dispatch)))))))

      :else
      (tools/execute-tool inner-backend tool-id args))))

(defn make-checkout-provisioning-backend
  ([inner-backend]
   (make-checkout-provisioning-backend inner-backend provision-frame! rollback-frame!))
  ([inner-backend provisioner-fn]
   (make-checkout-provisioning-backend inner-backend provisioner-fn rollback-frame!))
  ([inner-backend provisioner-fn rollback-fn]
   (->CheckoutProvisioningBackend inner-backend provisioner-fn rollback-fn
                                  (atom nil) (atom []))))

(defn- post-park!
  [base payload]
  (let [request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create (str (str/replace base #"/$" "")
                                           "/api/alpha/park")))
                    (.header "content-type" "application/json")
                    (.POST (HttpRequest$BodyPublishers/ofString
                            (json/write-str payload)))
                    (.build))
        response (.send (HttpClient/newHttpClient) request
                        (HttpResponse$BodyHandlers/ofString))
        status (.statusCode response)
        body (json/read-str (.body response) :key-fn keyword)]
    (if (<= 200 status 299)
      body
      (throw (ex-info "park endpoint refused request"
                      {:status status :body body})))))

(defn- park-dispatch
  "Attach the engine-owned park receipt to a successful raw dispatch result.
  Parking is deliberately best-effort after dispatch: failure is retained as
  :park/error but never rewrites a successful bell into a failed dispatch."
  [dispatch-result tool-id opts conductor park-post-fn]
  (let [job-id (:job-id dispatch-result)]
    (if-not (and (map? conductor) (:agent conductor) job-id)
      dispatch-result
      ;; Deadline default at CONSUMPTION (the bounded-recall lesson, 524cc42f)
      ;; and computed INSIDE the guard's reach: a malformed conductor must
      ;; degrade to :park/error, never crash the dispatch it decorates.
      (let [deadline-ms (+ (System/currentTimeMillis)
                           (* 1000 (long (or (:park-deadline-s conductor)
                                             2700))))
            payload {:agent (:agent conductor)
                     :session (:session conductor)
                     :surface (:surface conductor)
                     :cycle-id (:conductor/cycle-id opts)
                     :version (:conductor/version opts)
                     :awaiting [job-id]
                     :deadline-ms deadline-ms
                     :payload (or (:park-payload opts)
                                  (str "Await " (name tool-id)
                                       " job " job-id))}]
        (try
          (let [park-result (park-post-fn (:park-base conductor) payload)]
            (assoc dispatch-result :park/id (:id park-result)))
          (catch Throwable t
            (assoc dispatch-result :park/error
                   {:message (.getMessage t)})))))))

(defrecord GroundControlBackend [inner-backend dispatch-fn park-post-fn cycle-context]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (cond
      (= tool-id :guide-solver)
      (let [measured (last args)
            [opts packet] (butlast args)
            solver-seat (:solver-seat measured)
            bell-type (:bell-type opts)
            regime (:guidance-regime measured)]
        (cond
          (not (and (string? solver-seat) (not (str/blank? solver-seat))))
          {:ok false :error "guide-solver has no registered solver seat"}

          (nil? bell-type)
          {:ok false :error "guide-solver requires :bell-type"}

          (and (set? regime) (not (contains? regime bell-type)))
          {:ok false
           :error {:failure :guidance-type-off-regime
                   :bell-type bell-type
                   :guidance-regime regime}}

          :else
          (let [dispatch-result
                (try (dispatch-fn (assoc (or opts {}) :to solver-seat) packet)
                     (catch Throwable t t))]
            (if (instance? Throwable dispatch-result)
              {:ok false
               :error (str "guide-solver dispatch failed: "
                           (.getMessage ^Throwable dispatch-result))}
              {:ok true
               :result (-> (park-dispatch dispatch-result tool-id opts
                                          (:conductor @cycle-context)
                                          park-post-fn)
                           (assoc :ground-control/recipient solver-seat
                                  :ground-control/cycle (:cycle/id measured)
                                  :ground-control/type bell-type))}))))

      (contains? dispatch-channels tool-id)
      (let [[opts packet] (take 2 args)
            measured (last args)
            memory-channel (get dispatch-channels tool-id)
            solver-config (when (= :dispatch-solver tool-id)
                            (:solver-config measured))
            student-runner-budget
            (when (= :dispatch-student-fresh tool-id)
              (or (:student-runner-budget measured)
                  {:wall-clock-minutes 60}))
            scribe-seat (when (= :dispatch-scribe tool-id)
                          (:scribe-seat measured))
            ;; assoc LAST: the role fixes the channel and a caller cannot
            ;; override it.  A caller-supplied :push to the student would be a
            ;; containment breach, so this precedence is load-bearing.
            sent-opts (cond-> (merge
                               {:base dispatch-with-recall/default-agency-base}
                               (or opts {}))
                        solver-config (merge solver-config)
                        student-runner-budget
                        (assoc :student-runner-budget student-runner-budget
                               :timeout-ms
                               (* 60 1000
                                  (:wall-clock-minutes student-runner-budget)))
                        scribe-seat (assoc :to scribe-seat)
                        (= :dispatch-student-fresh tool-id)
                        (assoc :eligible-memory-ids
                               (:eligible-memory-ids measured)
                               :eligible-memory-provenance
                               (:eligible-memory-provenance measured))
                        true (assoc :memory-channel memory-channel))
            dispatch-result (try (dispatch-fn sent-opts
                                              packet)
                                 (catch Throwable t t))]
        (if (instance? Throwable dispatch-result)
          ;; A failed BELL is not a failed recall.  Recall is best-effort and
          ;; degrades to an empty offer; the bell is the work itself, so it must
          ;; surface as a structured tool failure rather than escape as an
          ;; exception and break the ToolBackend contract.
          {:ok false
           :error (str "ground-control dispatch failed: "
                       (.getMessage ^Throwable dispatch-result))}
          ;; This is the receipt emitted by the dispatcher that made the offer,
          ;; not a second account synthesized by the cycle machine.  Even an
          ;; empty/failed recall has one offered receipt.
          (let [receipt-body (get-in dispatch-result [:evidence :body])
                eligible-ids (:eligible-memory-ids receipt-body)
                retrieved-ids
                (get-in receipt-body
                        [:memory-use :memory-use/surfaced-ids])
                cycle-id (:cycle/id measured)
                step-index (:cycle/step-index measured)
                retrieval-probe
                (when (and cycle-id (integer? step-index)
                           (sequential? eligible-ids)
                           (sequential? retrieved-ids))
                  {:rprobe/id (str "rprobe/" cycle-id "/" step-index)
                   :rprobe/cycle cycle-id
                   :rprobe/available-ids (vec eligible-ids)
                   :rprobe/retrieved-ids (vec retrieved-ids)})]
            {:ok true
             :result (cond-> (assoc (park-dispatch dispatch-result tool-id opts
                                                   (:conductor @cycle-context)
                                                   park-post-fn)
                                    :memory-offers [(:evidence dispatch-result)])
                       solver-config
                       (assoc :ground-control/solver-config solver-config
                              :ground-control/recipient (:to sent-opts)
                              :ground-control/cycle cycle-id)
                       student-runner-budget
                       (assoc :ground-control/student-runner-budget
                              student-runner-budget
                              :ground-control/recipient (:to sent-opts)
                              :ground-control/cycle cycle-id)
                       scribe-seat
                       (assoc :ground-control/recipient scribe-seat
                              :ground-control/cycle cycle-id)
                       retrieval-probe
                       (assoc :retrieval-probe retrieval-probe))})))

      :else
      (tools/execute-tool inner-backend tool-id args))))

(defn make-ground-control-backend
  "Wrap a cycle backend with real ground-control dispatches.

  Dispatch tools take `[opts packet]`.  The role fixes the memory channel:
  solver is `:push+pull`; student is `:pull-only`.  `dispatch-fn` is injectable
  so envelope tests never bell a live agent."
  ([inner-backend]
   (make-ground-control-backend inner-backend
                                dispatch-with-recall/run-dispatch!))
  ([inner-backend dispatch-fn]
   (make-ground-control-backend inner-backend dispatch-fn post-park! (atom nil)))
  ([inner-backend dispatch-fn park-post-fn cycle-context]
   (->GroundControlBackend inner-backend dispatch-fn park-post-fn cycle-context)))

(def ^:private harness-paths
  ;; What "the harness" IS, for the purpose of freezing it: the retrieval and
  ;; collection machinery, the provisioner, and the dependency set. NOT the
  ;; mission record, the registration, the READMEs or the labs.
  ["src" "scripts" "deps.edn"])

(defn measure-harness-repository
  "Measure the harness revision as the commit that last changed the harness
  PATHS -- not the repository HEAD.

  HEAD moves on every note, mission-record entry and registration amendment, so
  a HEAD-scoped pin is stale the moment it is written: the amendment that
  records the pin is itself a commit, and a commit cannot contain its own sha.
  Path-scoping makes the pin mean what it says -- this harness code ran -- and
  keeps it stable while the surrounding prose moves. Same construction the
  validator already uses for the Lean (`prereg/lean-source-revision`)."
  [repo]
  (when-not (and (string? repo) (not (str/blank? repo)))
    (throw (ex-info "begin-problem-cycle requires :harness-repo" {})))
  (let [{rev-exit :exit rev-out :out rev-err :err}
        (apply shell/sh "git" "-C" repo "log" "-n" "1" "--format=%H" "--"
               harness-paths)
        {status-exit :exit status-out :out status-err :err}
        (apply shell/sh "git" "-C" repo "status" "--porcelain" "--"
               harness-paths)
        revision (str/trim rev-out)]
    (when-not (and (zero? rev-exit)
                   (re-matches #"[0-9a-f]{40}" revision))
      (throw (ex-info "failed to measure harness revision"
                      {:repo repo :exit rev-exit :error rev-err})))
    (when-not (zero? status-exit)
      (throw (ex-info "failed to measure harness tree status"
                      {:repo repo :exit status-exit :error status-err})))
    {:harness-revision revision
     :harness-tree-dirty? (not (str/blank? status-out))}))

(def ^:private substrate-page-limit
  ;; The substrate's own maximum. Asking for more is a hard 400
  ;; (:reason :invalid-limit, :maximum 5000), not a clamp -- so a larger number
  ;; here does not read more rows, it reads NONE. 10000 was the previous value
  ;; and it made :snapshot-store fail on every production call.
  5000)

(defn- snapshot-reviewed-memories []
  (let [rows (substrate/hyperedges-by-type :memory/assert
                                           {:limit substrate-page-limit})]
    ;; The store snapshot IS the measured transfer channel for the round. A
    ;; silently truncated snapshot is a silently wrong measurement, and the
    ;; endpoint offers no cursor -- so a full page cannot be distinguished from
    ;; a complete read and must refuse rather than under-report.
    (when (>= (count rows) substrate-page-limit)
      (throw (ex-info "store snapshot may be truncated"
                      {:rows (count rows) :limit substrate-page-limit})))
    (->> rows
         (keep #(get-in % [:hx/props :roles :entry]))
         (filter string?)
         distinct
         sort
         vec)))

(defn- read-frozen-registration [path]
  (when-not (and (string? path) (not (str/blank? path)))
    (throw (ex-info "read-registration requires a registration path" {})))
  (edn/read-string (slurp (io/file path))))

(defn- validate-frozen-registration [registration]
  (let [failures (prereg/registration-shape-failures registration)]
    {:registration registration
     :failures failures
     :valid? (empty? failures)}))

(defn- read-substrate-page [hx-type options]
  (let [limit (long (or (:limit options) substrate-page-limit))]
    (when (> limit substrate-page-limit)
      (throw (ex-info "read-substrate limit exceeds substrate maximum"
                      {:limit limit :maximum substrate-page-limit})))
    (let [rows (substrate/hyperedges-by-type hx-type
                                             (assoc (or options {}) :limit limit))]
      (when (>= (count rows) limit)
        (throw (ex-info "read-substrate page may be truncated"
                        {:rows (count rows) :limit limit})))
      {:type hx-type :rows (vec rows) :complete? true})))

(defn- attempt-by-id [outputs attempt-id]
  (some #(when (= attempt-id (:attempt/id %)) %)
        (cons (:solver-attempt outputs) (:student-attempts outputs))))

(defrecord ProblemCycleBackend [inner-backend harness-measurer cycle-context
                                begin-seq active-cycle-id
                                store-snapshotter clock record-memory-fn]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (cond
      (= tool-id :begin-problem-cycle)
      ;; DETERMINISTIC BUT NOT CONSTANT. The id must be reproducible, because
      ;; save/load state files are keyed by it (data/problem-state/<cycle-id>/,
      ;; write-once). It must ALSO differ between two cycles of the same session:
      ;; without the sequence number, a second cycle on the same problem with the
      ;; same begin args got a byte-identical id, so it would write into the first
      ;; cycle's version directory AND -- worse -- the :problem-load cross-cycle
      ;; guard compares :current-cycle-id and so could not fire, letting cycle 2
      ;; restore cycle 1's state as if it were its own.
      ;;
      ;; The counter is per-peripheral and starts at 0, so replaying the same
      ;; sequence of begins from a fresh peripheral reproduces the same ids.
      (let [{:keys [session-id problem-id harness-repo]} @cycle-context
            seq-no (dec (swap! begin-seq inc))
            identity-input [session-id problem-id seq-no (vec args)]
            digest (apm-harness/sha256-bytes
                    (.getBytes (pr-str identity-input) "UTF-8"))
            cycle-id (str problem-id "-" digest)]
        (reset! active-cycle-id cycle-id)
        {:ok true
         :result (merge {:cycle/id cycle-id
                         :cycle/opened-at (now-string)}
                        (harness-measurer harness-repo))})

      (= tool-id :snapshot-store)
      (let [cycle-id @active-cycle-id]
        (if cycle-id
          {:ok true
           :result {:snap/id (str "snap/" cycle-id)
                    :snap/cycle cycle-id
                    :snap/memory-ids (vec (store-snapshotter))}}
          {:ok false :error "snapshot-store has no open cycle"}))

      (= tool-id :freeze-stratum)
      (let [measured (long (or (:cycle/step-index (last args)) (clock)))]
        {:ok true :result {:cycle/stratum-frozen-at measured}})

      (= tool-id :read-registration)
      (try
        {:ok true :result (read-frozen-registration (first args))}
        (catch Throwable t
          {:ok false :error (str "read-registration failed: " (.getMessage t))}))

      (= tool-id :validate-registration)
      {:ok true :result (validate-frozen-registration (first args))}

      (= tool-id :read-substrate)
      (try
        {:ok true :result (read-substrate-page (first args) (second args))}
        (catch Throwable t
          {:ok false :error (str "read-substrate failed: " (.getMessage t))}))

      (= tool-id :read-attempt-result)
      (let [attempt-id (first args)
            outputs (:cycle/outputs (last args))]
        (if-let [attempt (attempt-by-id outputs attempt-id)]
          {:ok true :result attempt}
          {:ok false :error (str "attempt result not found: " attempt-id)}))

      (= tool-id :write-substrate)
      (let [cycle-id @active-cycle-id]
        (if-not cycle-id
          {:ok false :error "write-substrate has no open cycle"}
          (let [ctx {:agent-id (or (:author @cycle-context)
                                   "problem-peripheral")
                     :via "problem-peripheral"
                     :session-id (:session-id @cycle-context)
                     :domain :mathematics
                     :evidence-store
                     (f1b/make-futon1b-backend (substrate/configured-url))}
                receipt (try (record-memory-fn ctx (first args))
                             (catch Throwable t
                               {:ok false
                                :error {:error/component :E-store
                                        :error/code :memory-write-threw
                                        :error/message (.getMessage t)}}))]
            (if (:ok receipt)
              {:ok true
               :result {:memory-id (:id receipt) :cycle cycle-id}}
              {:ok false
               :error (str "write-substrate failed: "
                           (pr-str (:error receipt)))}))))

      (= tool-id :assign-checkouts)
      (let [result (tools/execute-tool inner-backend tool-id args)]
        (if (:ok result)
          (assoc-in result [:result :assigned-at]
                    (long (or (:cycle/step-index (last args)) (clock))))
          result))

      (= tool-id advance)
      (let [current-phase (:cycle/current-phase (last args))
            current-index (.indexOf phase-order current-phase)
            next-phase (when (<= 0 current-index)
                         (get phase-order (inc current-index)))]
        (if next-phase
          {:ok true :result {:cycle/phase next-phase}}
          {:ok false :error (str "advance-problem-phase has no open phase: "
                                 current-phase)}))

      (= tool-id :emit-frame)
      (let [options (first args)
            cycle-id (:cycle/id (last args))
            scaffold-path (:scaffold-path options)
            closing-path (:closing-path options)
            witness-path (:containment-witness-path options)
            scaffold-at (.toMillis
                         (Files/getLastModifiedTime
                          (.toPath (io/file (str scaffold-path)))
                          (make-array java.nio.file.LinkOption 0)))
            closing-at (.toMillis
                        (Files/getLastModifiedTime
                         (.toPath (io/file (str closing-path)))
                         (make-array java.nio.file.LinkOption 0)))
            scaffold-hash (apm-harness/sha256-bytes
                           (apm-harness/file-bytes scaffold-path))
            closing-hash (apm-harness/sha256-bytes
                          (apm-harness/file-bytes closing-path))
            frame-id (str "frame/" cycle-id)
            witness-recorded? (and (some? witness-path)
                                   (.isFile (io/file (str witness-path))))]
        (when-not (< scaffold-at closing-at)
          (throw (ex-info "frame snapshots were not taken at distinct times"
                          {:scaffold-path (str scaffold-path)
                           :scaffold-at scaffold-at
                           :closing-path (str closing-path)
                           :closing-at closing-at})))
        {:ok true
         :result
         {:frame {:frame/id frame-id
                  :frame/cycle cycle-id
                  :frame/scaffold-hash scaffold-hash
                  :frame/closing-hash closing-hash
                  :frame/scaffold-at scaffold-at
                  :frame/closing-at closing-at
                  :frame/keys [:scaffold :closing]}
          :containment-probe
          {:cprobe/id (str "cprobe/" frame-id)
           :cprobe/frame frame-id
           :cprobe/claimed? (true? (:containment-claimed? options))
           :cprobe/recorded? witness-recorded?
           ;; :cprobe/passed? WITH the question mark. derive-trace reads
           ;; :cprobe/passed? (cycle_harness.clj:126); this emitted :cprobe/passed,
           ;; so :containment-probe-passed? was ALWAYS nil and F8 fired on every
           ;; claimed containment no matter how well witnessed. The F8 test could
           ;; not see it because it only checked that the gate FIRES.
           :cprobe/passed? witness-recorded?}}})

      (= tool-id :write-disposition)
      (let [options (or (first args) {})
            cycle-id @active-cycle-id
            step-index (:cycle/step-index (last args))
            outcome (or (:disp/outcome options) (:outcome options))
            allowed #{:closed :tier-a :tier-b :defective}
            residual-keys (filter #(contains? options %)
                                  [:disp/residual-sorries :residual-sorries])
            axiom-keys (filter #(contains? options %)
                               [:disp/axiom-clean? :axiom-clean?])
            invalid-residual
            (some #(when-not (nat-int? (get options %)) %) residual-keys)
            invalid-axiom
            (some #(when-not (boolean? (get options %)) %) axiom-keys)
            residual (if (contains? options :disp/residual-sorries)
                       (:disp/residual-sorries options)
                       (:residual-sorries options))
            axiom-clean? (if (contains? options :disp/axiom-clean?)
                           (:disp/axiom-clean? options)
                           (:axiom-clean? options))]
        (cond
          invalid-residual
          {:ok false
           :error {:error/code :invalid-disposition-field
                   :field invalid-residual
                   :expected :nonnegative-integer}}

          invalid-axiom
          {:ok false
           :error {:error/code :invalid-disposition-field
                   :field invalid-axiom
                   :expected :boolean}}

          (and cycle-id (contains? allowed outcome))
          {:ok true
           :result (cond-> {:disp/id (str "disp/" cycle-id "/" step-index)
                            :disp/cycle cycle-id
                            :disp/outcome outcome
                            :disp/step-index step-index}
                     (seq residual-keys)
                     (assoc :disp/residual-sorries residual)

                     (seq axiom-keys)
                     (assoc :disp/axiom-clean? axiom-clean?))}

          :else
          {:ok false
           :error (str "write-disposition requires an open cycle and one of "
                       allowed)}))

      (= tool-id :write-use)
      (let [options (first args)
            cycle-id @active-cycle-id
            step-index (:cycle/step-index (last args))
            offer-id (or (:use/offer options) (:offer-id options))]
        (if (and cycle-id (string? offer-id) (not (str/blank? offer-id)))
          {:ok true
           :result {:use/id (str "use/" cycle-id "/" step-index)
                    :use/offer offer-id}}
          {:ok false :error "write-use requires an open cycle and :offer-id"}))

      (= tool-id :promote-artifact)
      (let [options (first args)
            cycle-id @active-cycle-id
            step-index (:cycle/step-index (last args))
            memory-id (:memory-id options)
            artifact-id (or (:promo/artifact-id options) (:artifact-id options)
                            memory-id)
            importable? (true? (if (contains? options :promo/importable)
                                 (:promo/importable options)
                                 (:importable? options)))
            need-tags (vec (or (:promo/need-tags options)
                               (:need-tags options) []))
            review-result
            (when (and cycle-id memory-id)
              (try
                (memory-lifecycle/promote-memory-attachment!
                 {:agent-id (:reviewer options)
                  :acting-identity (:acting-identity options)
                  :session-id (:session-id @cycle-context)
                  :cycle-id cycle-id
                  :domain :mathematics
                  :evidence-store
                  (f1b/make-futon1b-backend (substrate/configured-url))}
                 (select-keys options [:memory-id :pattern-id :reviewer
                                       :pattern-ids :verdict
                                       :review-evidence-id]))
                (catch Throwable t
                  {:ok false
                   :finding {:failure :promotion-attachment-review-threw
                             :message (.getMessage t)}})))]
        (cond
          (not (and cycle-id (string? artifact-id)
                    (not (str/blank? artifact-id))))
          {:ok false
           :error "promote-artifact requires an open cycle and :artifact-id"}

          (and memory-id (not (:ok review-result)))
          {:ok false
           :error (let [finding (:finding review-result)]
                    (assoc finding :error/code (:failure finding)))}

          :else
          {:ok true
           :result (cond->
                    {:promo/id (str "promo/" cycle-id "/" step-index)
                     :promo/cycle cycle-id
                     :promo/artifact-id artifact-id
                    ;; ONE key, the one derive-trace actually reads
                    ;; (cycle_harness.clj:136). Emitting both spellings was a
                    ;; hedge against not knowing which is canonical, and a hedge
                    ;; is a second route to a field the machine owns: if the two
                    ;; ever disagree, which wins depends on the consumer you ask.
                    ;; The `?` spelling matches every other boolean on these
                    ;; entities -- :cprobe/claimed?, :cprobe/recorded?,
                    ;; :cprobe/passed? -- and the last of those was silently wrong
                    ;; for exactly this reason.
                     :promo/importable? importable?
                     :promo/need-tags need-tags}
                     memory-id
                     (assoc :promo/pattern-id (:pattern-id options)
                            :promo/review-evidence-id
                            (:review-evidence-id review-result)
                            :promo/review-findings
                            (vec (:findings review-result))))}))

      :else
      (tools/execute-tool inner-backend tool-id args))))

(defn make-problem-cycle-backend
  ([inner-backend harness-measurer cycle-context store-snapshotter clock]
   (make-problem-cycle-backend inner-backend harness-measurer cycle-context
                               store-snapshotter clock
                               memory-write/record-memory!))
  ([inner-backend harness-measurer cycle-context store-snapshotter clock
    record-memory-fn]
   (->ProblemCycleBackend inner-backend harness-measurer cycle-context
                          (atom 0) (atom nil)
                          store-snapshotter clock record-memory-fn)))

(defrecord EvidenceRequiredProblemPeripheral [inner]
  runner/PeripheralRunner
  (start [_ context]
    (or (runner/validate-context :problem context
                                 #{:session-id :evidence-store})
        (when-not (or (instance? clojure.lang.IAtom (:evidence-store context))
                      (satisfies? evidence-backend/EvidenceBackend
                                  (:evidence-store context)))
          (runner/runner-error :problem :invalid-context
                               ":evidence-store must be an evidence store"
                               :field :evidence-store))
        (runner/start inner context)))
  (step [_ state action]
    (runner/step inner state action))
  (stop [_ state reason]
    (runner/stop inner state reason)))

(defn make-problem
  ([] (make-problem (tools/make-mock-backend)))
  ([backend]
   (make-problem backend dispatch-with-recall/run-dispatch!
                 default-problem-state-root provision-frame!))
  ([backend dispatch-fn]
   (make-problem backend dispatch-fn default-problem-state-root provision-frame!))
  ([backend dispatch-fn state-root]
   (make-problem backend dispatch-fn state-root provision-frame!))
  ([backend dispatch-fn state-root provisioner-fn]
   (make-problem backend dispatch-fn state-root provisioner-fn
                 measure-harness-repository))
  ([backend dispatch-fn state-root provisioner-fn harness-measurer]
   (make-problem backend dispatch-fn state-root provisioner-fn harness-measurer
                 snapshot-reviewed-memories #(System/nanoTime)))
  ([backend dispatch-fn state-root provisioner-fn harness-measurer
    store-snapshotter clock]
   (make-problem backend dispatch-fn state-root provisioner-fn harness-measurer
                 store-snapshotter clock memory-write/record-memory!))
  ([backend dispatch-fn state-root provisioner-fn harness-measurer
    store-snapshotter clock record-memory-fn]
   (let [cycle-context (atom nil)]
     (->EvidenceRequiredProblemPeripheral
      (cycle/make-cycle-peripheral
       (assoc problem-domain-config :cycle-context cycle-context) problem-spec
       (make-problem-cycle-backend
        (make-checkout-provisioning-backend
         (make-ground-control-backend
          (make-problem-state-backend backend state-root)
          dispatch-fn post-park! cycle-context)
         provisioner-fn)
        harness-measurer cycle-context store-snapshotter clock
        record-memory-fn))))))
