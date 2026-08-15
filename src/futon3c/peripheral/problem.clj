(ns futon3c.peripheral.problem
  "Problem peripheral — one registered experimental problem per cycle."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.cycle-harness :as apm-harness]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.tools :as tools]
            [futon3c.substrate.client :as substrate])
  (:import [java.nio.file Files StandardCopyOption]
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
  [:register :frame :guided-solve :intervene :student-attempts
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
   :student-attempts #{:dispatch-student-fresh :read-attempt-result advance}
   :adjudicate #{:write-disposition :write-use advance}
   :promote #{:promote-artifact advance}
   :close #{:record-measurement :emit-capability-probes :emit-trace :validate-trace
            :write-authorization advance}
   :completed #{}})

(def required-outputs
  {:register #{:registration :store-snapshot :stratum-frozen-at
               :environment-revision :harness-revision :environment-checkouts}
   :frame #{:frame :containment-probe}
   :guided-solve #{:solver-attempt :ground-control-events :memory-offers}
   :intervene #{:intervention}
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
    {:snapshot/subject {:ref/type :peripheral :ref/id "problem"}
     :snapshot/body {:snapshot :problem-save
                     :problem-id (:problem-id state)
                     :cycle-id (:current-cycle-id state)
                     :version (:version result)
                     :phase (:current-phase state)
                     :cycles-completed (:cycles-completed state)
                     :step-count (count (:steps state))}
     :snapshot/tags [:problem :snapshot]}))

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

(defn- recorded-frame-output [state]
  (some-> (recorded-tool-result state :emit-frame)
          (select-keys [:frame :containment-probe])))

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

(declare recorded-measurement recorded-close-envelope recorded-validation
         recorded-authorization)

(defn- stamp-environment-outputs [state payload]
  (let [closing? (= :close (:current-phase state))
        payload (if closing?
                  ;; Close artifacts are retained derived-step results, never
                  ;; advance-payload assertions from the caller.
                  ;; :retrieval-probes is stripped for the opposite reason to the
                  ;; others: they are replaced by recorded derived results, but
                  ;; NOTHING produces retrieval probes, so an injected
                  ;; `:retrieval-probes []` would read as "the producer ran and
                  ;; returned nothing" and silence the one gap that has no producer.
                  ;; Verified: injecting it removed :missing-trace-entity-producer
                  ;; for :retrieval-probe from the close envelope.
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

      (some? frozen-at)
      (assoc :stratum-frozen-at frozen-at)

      (some? assigned-at)
      (assoc :assigned-at assigned-at)

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

(defn- thread-current-phase [state tool args]
  (case tool
    :advance-problem-phase
    (conj (vec args) {:cycle/current-phase (:current-phase state)})

    :emit-frame
    (conj (vec args) {:cycle/id (:current-cycle-id state)})

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
  ;; These pairings are deliberately narrower than "a nearby phase". Three
  ;; required capabilities currently have no step that exercises them:
  ;; need-retrieval has no probe producer. It gets no synthetic probe.
  {:created-frame-worked :emit-frame
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
      (assoc "locked-lemma exposure" locked-exposure))))

(defn- unset-measurement-reason [field]
  (case field
    "statement defects at review"
    "unset: no formalizer review record is present in cycle outputs"
    "attempts or closer hops"
    "unset: Agency-derived guidance evidence is not available to this tool"
    "memories promoted"
    "unset: promotion outputs do not identify which artifacts are memories"
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
        ;; Capability probes are never accepted from the advance payload. Only
        ;; the engine-derived tool's retained step result can supply them.
        ;; :retrieval-probes joins the strip list for the OPPOSITE reason to the
        ;; other two: they are replaced by recorded derived results, but NOTHING
        ;; produces retrieval probes -- so an injected `:retrieval-probes []` would
        ;; read as "the producer ran and returned nothing" and silence the one gap
        ;; that has no producer. Verified before this fix: injecting it removed
        ;; :retrieval-probe from the close envelope's producer failures.
        outputs (cond-> (dissoc (:cycle/outputs state)
                                :capability-probes :measurement :retrieval-probes)
                  (some? recorded-probes)
                  (assoc :capability-probes recorded-probes)
                  (some? measurement)
                  (assoc :measurement measurement))
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
                        (assoc :measurement-summary
                               {:measured (count (get-in outputs
                                                        [:measurement :meas/values]))
                                :unset (count (get-in outputs
                                                     [:measurement :meas/unset]))}))]
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
                   :emit-capability-probes emit-capability-probes-from-state
                   :emit-trace emit-trace-from-state
                   :validate-trace validate-trace-from-state
                   :write-authorization write-authorization-from-state}
   :output-invariants
   [{:id :harness-tree-clean
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
   :dispatch-student-fresh :pull-only})

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
        {:keys [exit err]} (apply shell/sh command)]
    (when-not (zero? exit)
      (throw (ex-info "frames.bb open failed"
                      {:exit exit :error err :frame-id frame-id})))
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
      (let [[opts packet] args
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
              (let [dispatch (tools/execute-tool
                              inner-backend tool-id
                              [(assoc (or opts {})
                                      :environment-checkout (:checkout frame)
                                      :environment-revision (:base-revision frame))
                               packet])]
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

(defrecord GroundControlBackend [inner-backend dispatch-fn]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (if-let [memory-channel (get dispatch-channels tool-id)]
      (let [[opts packet] args
            ;; assoc LAST: the role fixes the channel and a caller cannot
            ;; override it.  A caller-supplied :push to the student would be a
            ;; containment breach, so this precedence is load-bearing.
            dispatch-result (try (dispatch-fn (assoc (or opts {})
                                                     :memory-channel memory-channel)
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
          {:ok true
           :result (assoc dispatch-result
                          :memory-offers [(:evidence dispatch-result)])}))
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
   (->GroundControlBackend inner-backend dispatch-fn)))

(def ^:private harness-paths
  ;; What "the harness" IS, for the purpose of freezing it: the retrieval and
  ;; collection machinery, the provisioner, and the dependency set. NOT the
  ;; mission record, the registration, the READMEs or the labs.
  ["src" "scripts" "deps.edn"])

(defn- measure-harness-repository
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

(defrecord ProblemCycleBackend [inner-backend harness-measurer cycle-context
                                begin-seq active-cycle-id
                                store-snapshotter clock]
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
      (let [measured (long (clock))]
        {:ok true :result {:cycle/stratum-frozen-at measured}})

      (= tool-id :assign-checkouts)
      (let [result (tools/execute-tool inner-backend tool-id args)]
        (if (:ok result)
          (assoc-in result [:result :assigned-at] (long (clock)))
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
           :cprobe/passed witness-recorded?}}})

      :else
      (tools/execute-tool inner-backend tool-id args))))

(defn make-problem-cycle-backend
  [inner-backend harness-measurer cycle-context store-snapshotter clock]
  (->ProblemCycleBackend inner-backend harness-measurer cycle-context
                         (atom 0) (atom nil)
                         store-snapshotter clock))

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
   (let [cycle-context (atom nil)]
     (cycle/make-cycle-peripheral
      (assoc problem-domain-config :cycle-context cycle-context) problem-spec
      (make-problem-cycle-backend
       (make-checkout-provisioning-backend
        (make-ground-control-backend
         (make-problem-state-backend backend state-root)
         dispatch-fn)
        provisioner-fn)
       harness-measurer cycle-context store-snapshotter clock)))))
