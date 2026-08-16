(ns futon3c.apm.preregistration
  "Concrete runtime specification aligned by hand with
  DarkTower.APMDemonstration.round1Registration.  This is not a generator or
  a formal Lean-to-Clojure projection."
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.time Duration]))

(def required-lean-revision "4331becf6f0c8471537adcc264a8fde7e12c94c9")
(def required-lean-source "DarkTower/APMDemonstrationPreregistration.lean")
(def required-lean-registration
  "DarkTower.APMDemonstration.round1Registration")

(def required-modules [:R :F :S :A :M :P :X])
(def required-structural-invariants [:F1])
(def required-runtime-invariants
  ;; F7's round-one definition makes "available" mean "retrieved", so the
  ;; predicate cannot fail. Do not preregister a tautology as a runtime check.
  [:F2 :F3 :F4 :F5 :F6 :F8 :F9])

(def required-capabilities
  ;; Launch gating is structural: the generic DarkTower Launch constructor
  ;; requires ReadyToRun, whose failed-obligation cases are uninhabited. There
  ;; is no APM runtime refusal event to attest.
  [:frame-containment-witnessed :created-frame-worked
   :unique-disposition :offer-use-disposition
   :need-retrieval :promotion-importable :promotion-need-taggable
   :measurement-populated])

(def required-measurement-fields
  ["statement defects at review" "terminal disposition"
   "residual executable sorries" "attempts or closer hops"
   "axiom cleanliness" "memories promoted" "review escape rate"
   "promoted then surfaced then used" "contract leaks"
   "duplicate declarations" "locked-lemma exposure" "promotion coverage"
   "unconsumed promotions" "import-only edges" "scribe lane coverage"
   "arc-lane yield" "rewrite rule offered and used"])

(def required-registration-keys
  [:kind :schema :lean-registration :lean-source :lean-revision :modules
   :structural-invariants :runtime-invariants :problem :variation
   :claim :arms :replication-stage :pilot-units :confirmation-units
   :estimated-cost :budget-cap :teardown-deadline :stop-rules :decision-rule
   :required-capabilities :required-measurement-fields :reg/role-cards
   :reg/solver-seat])

(def role-seat-keys
  {:solver :reg/solver-seat
   :guide :reg/guide-seat
   :proctor :reg/proctor-seat
   :scribe :reg/scribe-seat
   :student :reg/student-seat})

(def required-trace-keys
  [:problem :frame :cycle-closed?
   :disposition-ids :memory-offers :memory-disposition-offer-ids
   :stratum-frozen-at :assigned-at :cycle/attempts :cycle/mode
   :cycle/deposit-state :cycle/paired-with :cycle/store-snapshot-id
   :cycle/store-snapshot-memory-ids :cycle/window
   :denominator-declared? :denominator-inferred-from-corpus?
   :available-artifact-ids :need-probe-retrieved-ids :containment-claimed?
   :containment-probe-recorded? :containment-probe-passed?
   :capability-probes :required-measurement-fields
   :measurement :promoted-artifact-ids
   :importable-promoted-artifact-ids :need-tagged-promoted-artifact-ids])

(defn read-edn [path]
  (edn/read-string (slurp path)))

(defn nonblank-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn string-vector? [x]
  (and (vector? x) (every? string? x)))

(defn problem? [x]
  (and (map? x)
       (nonblank-string? (:problem-id x))
       (nonblank-string? (:difficulty-stratum x))
       (nonblank-string? (:regime x))
       (string-vector? (:locked-lemma-exposure x))))

(defn probe? [x]
  (and (map? x) (keyword? (:capability x))
       (nonblank-string? (:evidence-id x)) (boolean? (:recorded? x))))

(defn sha40? [x]
  (and (string? x) (boolean (re-matches #"[0-9a-f]{40}" x))))

(defn instant-string? [x]
  (and (nonblank-string? x)
       (try
         (java.time.Instant/parse x)
         true
         (catch Exception _ false))))

(defn attempt? [x]
  (and (map? x)
       (nonblank-string? (:cycle/regime x))
       (sha40? (:cycle/store-revision x))
       (sha40? (:cycle/harness-revision x))
       (nonblank-string? (:cycle/environment-checkout x))
       (sha40? (:cycle/environment-revision x))
       (boolean? (:cycle/runner-freshness x))))

(defn memory-offer? [x]
  (and (map? x)
       (nonblank-string? (:offer/id x))
       (nonblank-string? (:offer/memory-id x))))

(def guidance-bell-types
  "Agency typed-bell performatives permitted in a guidance regime."
  #{:query :answer :assert :challenge :agree :define :retract :suggest :request})

(defn registration-shape-failures [registration]
  (if-not (map? registration)
    [:registration-not-map]
    (let [role-cards (:reg/role-cards registration)
          guidance-regime (:reg/guidance-regime registration)
          carded-seat-failures
          (if (map? role-cards)
            (keep (fn [[role seat-key]]
                    (when (and (contains? role-cards role)
                               (not (nonblank-string? (get registration seat-key))))
                      {:finding :unstaffed-carded-seat
                       :role role
                       :seat-key seat-key}))
                  role-seat-keys)
            [])]
      (into
       (cond-> []
         (and (contains? registration :reg/guidance-regime)
              (not (and (set? guidance-regime)
                        (seq guidance-regime)
                        (every? guidance-bell-types guidance-regime))))
         (conj {:finding :invalid-guidance-regime
                :value guidance-regime})
         (some #(not (contains? registration %)) required-registration-keys)
         (conj :registration-missing-required-key)
         (not (problem? (:problem registration)))
         (conj :malformed-problem)
         (not (and (map? (:variation registration))
                   (#{:controlled :measured} (get-in registration [:variation :kind]))
                   (nonblank-string? (get-in registration [:variation :endpoint]))))
         (conj :malformed-variation)
         (not (number? (:estimated-cost registration)))
         (conj :malformed-estimated-cost)
         (not (number? (:budget-cap registration)))
         (conj :malformed-budget-cap)
         (not (or (nil? (:teardown-deadline registration))
                  (number? (:teardown-deadline registration))))
         (conj :malformed-teardown-deadline)
         (not (and (vector? (:stop-rules registration))
                   (seq (:stop-rules registration))
                   (every? keyword? (:stop-rules registration))))
         (conj :malformed-stop-rules)
         (not (and (map? (:decision-rule registration))
                   (keyword? (get-in registration [:decision-rule :id]))
                   (vector? (get-in registration [:decision-rule :outcomes]))
                   (seq (get-in registration [:decision-rule :outcomes]))
                   (every? keyword? (get-in registration
                                            [:decision-rule :outcomes]))))
         (conj :malformed-decision-rule)
         (not (and (map? role-cards)
                   (seq role-cards)
                   (every? (fn [[role hash]]
                             (and (keyword? role) (sha40? hash)))
                           role-cards)))
         (conj :malformed-role-cards)
         (and (map? role-cards)
              (contains? role-cards :guide)
              (contains? role-cards :proctor)
              (nonblank-string? (:reg/guide-seat registration))
              (= (:reg/guide-seat registration)
                 (:reg/proctor-seat registration)))
         (conj :guide-proctor-not-separated))
       carded-seat-failures))))

(defn trace-shape-failures [trace]
  (if-not (map? trace)
    [:trace-not-map]
    (cond-> []
      (some #(not (contains? trace %)) required-trace-keys)
      (conj :trace-missing-required-key)
      (not (problem? (:problem trace)))
      (conj :malformed-trace-problem)
      (not (and (map? (:frame trace))
                (nonblank-string? (get-in trace [:frame :scaffold-hash]))
                (nonblank-string? (get-in trace [:frame :closing-hash]))))
      (conj :malformed-frame)
      (not (and (vector? (:capability-probes trace))
                (every? probe? (:capability-probes trace))))
      (conj :malformed-capability-probes)
      (not (and (vector? (:cycle/attempts trace))
                (seq (:cycle/attempts trace))
                (every? attempt? (:cycle/attempts trace))))
      (conj :malformed-cycle-attempts)
      (not (#{:store-mode :harness-mode} (:cycle/mode trace)))
      (conj :malformed-cycle-mode)
      (not (#{:with-deposit :without-deposit :n/a}
             (:cycle/deposit-state trace)))
      (conj :malformed-deposit-state)
      (not (or (nil? (:cycle/paired-with trace))
               (nonblank-string? (:cycle/paired-with trace))))
      (conj :malformed-paired-with)
      (not (nonblank-string? (:cycle/store-snapshot-id trace)))
      (conj :malformed-store-snapshot-id)
      (not (string-vector? (:cycle/store-snapshot-memory-ids trace)))
      (conj :malformed-store-snapshot-membership)
      (not (and (map? (:cycle/window trace))
                (instant-string? (get-in trace [:cycle/window :opened-at]))
                (instant-string? (get-in trace [:cycle/window :closed-at]))))
      (conj :malformed-cycle-window)
      (not (and (vector? (:memory-offers trace))
                (every? memory-offer? (:memory-offers trace))))
      (conj :malformed-memory-offers)
      (not (and (map? (:measurement trace))
                (map? (get-in trace [:measurement :meas/values]))
                (every? string? (keys (get-in trace
                                              [:measurement :meas/values])))
                (map? (get-in trace [:measurement :meas/unset]))
                (every? (fn [[field reason]]
                          (and (string? field) (nonblank-string? reason)))
                        (get-in trace [:measurement :meas/unset]))))
      (conj :malformed-measurement)
      (not (every? #(or (true? (get trace %)) (false? (get trace %)))
                   [:cycle-closed?
                    :denominator-declared? :denominator-inferred-from-corpus?
                    :containment-claimed? :containment-probe-recorded?
                    :containment-probe-passed?]))
      (conj :malformed-trace-boolean))))

(defn lean-source-revision
  "Return the commit which last changed the named Lean source, or nil.  The
  repository location is an invocation input, not part of the registration."
  [lean-repo]
  (let [{:keys [exit out]}
        (shell/sh "git" "-C" lean-repo "log" "-n" "1" "--format=%H" "--"
                  required-lean-source)]
    (when (zero? exit) (not-empty (str/trim out)))))

(defn registration-content-failures [registration actual-lean-revision]
  (cond-> []
    (not= :apm-demonstration-round1-registration (:kind registration))
    (conj :wrong-kind)
    (not= 1 (:schema registration))
    (conj :wrong-schema)
    (not= required-lean-registration (:lean-registration registration))
    (conj :wrong-lean-registration)
    (not= required-lean-source (:lean-source registration))
    (conj :wrong-lean-source)
    (not= required-lean-revision (:lean-revision registration))
    (conj :wrong-lean-revision)
    (nil? actual-lean-revision)
    (conj :lean-source-revision-unavailable)
    (and actual-lean-revision
         (not= required-lean-revision actual-lean-revision))
    (conj :stale-lean-revision)
    (not= required-modules (:modules registration))
    (conj :wrong-modules)
    (not= required-structural-invariants (:structural-invariants registration))
    (conj :wrong-structural-invariants)
    (not= required-runtime-invariants (:runtime-invariants registration))
    (conj :wrong-runtime-invariants)
    (not= :descriptive (:claim registration))
    (conj :wrong-claim)
    (not= [{:name "one problem, one-shot measurement"
            :neutral? false :axes [] :role :treatment}]
          (:arms registration))
    (conj :wrong-arms)
    (not= :pilot (:replication-stage registration))
    (conj :wrong-replication-stage)
    (not= [(:problem registration)] (:pilot-units registration))
    (conj :wrong-pilot-units)
    (not= [] (:confirmation-units registration))
    (conj :pilot-has-confirmation-units)
    (not= required-capabilities (:required-capabilities registration))
    (conj :wrong-required-capabilities)
    (not= required-measurement-fields (:required-measurement-fields registration))
    (conj :wrong-required-measurement-fields)
    (and (number? (:estimated-cost registration))
         (number? (:budget-cap registration))
         (> (:estimated-cost registration) (:budget-cap registration)))
    (conj :over-budget)))

(defn subset? [xs ys]
  (every? (set ys) xs))

(defn exactly-one? [xs]
  (= 1 (count xs)))

(defn measurement-fields [trace]
  (let [measurement (:measurement trace)]
    (concat (keys (:meas/values measurement))
            (keys (:meas/unset measurement)))))

(defn memory-offer-ids [trace]
  (map :offer/id (:memory-offers trace)))

(defn surfaced-memory-ids [trace]
  (map :offer/memory-id (:memory-offers trace)))

(defn attempt-values [trace field]
  (map field (:cycle/attempts trace)))

(defn fetch-agency-jobs
  "Read independently recorded dispatch jobs.  The complete endpoint,
  including its caller-chosen limit, is an invocation input."
  [endpoint]
  (try
    (let [client (-> (HttpClient/newBuilder)
                     (.connectTimeout (Duration/ofSeconds 3))
                     (.build))
          request (-> (HttpRequest/newBuilder (URI/create endpoint))
                      (.timeout (Duration/ofSeconds 5))
                      (.GET)
                      (.build))
          response (.send client request (HttpResponse$BodyHandlers/ofString))]
      (if (= 200 (.statusCode response))
        (let [body (json/read-str (.body response) :key-fn keyword)]
          (if (vector? (:jobs body))
            {:status :ok :jobs (:jobs body)}
            {:status :unavailable :reason :missing-jobs-array}))
        {:status :unavailable :reason :http-status
         :http-status (.statusCode response)}))
    (catch Exception e
      {:status :unavailable :reason :request-failed
       :message (.getMessage e)})))

(defn inside-cycle-window?
  "Whether an independently timestamped record falls in the trace window."
  [trace record]
  (let [{:keys [opened-at closed-at]} (:cycle/window trace)
        timestamp (or (:created-at record) (:started-at record))]
    (try
      (when (instant-string? timestamp)
        (let [opened (java.time.Instant/parse opened-at)
              closed (java.time.Instant/parse closed-at)
              at (java.time.Instant/parse timestamp)]
          (and (not (.isBefore at opened))
               (not (.isAfter at closed)))))
      (catch Exception _ false))))

(defn direct-channel-inside-window? [trace jobs]
  (some #(and (string? (:caller %))
              (string? (:agent-id %))
              (str/starts-with? (:caller %) "claude-")
              (str/starts-with? (:agent-id %) "zai-")
              (inside-cycle-window? trace %))
        jobs))

(defn guidance-count
  "Count in-window dispatches to solver, less machine-recorded openings.

  Caller is deliberately ignored: it is client-authored and therefore cannot
  be a trustworthy part of the measurement predicate."
  [trace jobs solver-seat]
  (- (count (filter #(and (= solver-seat (:agent-id %))
                          (inside-cycle-window? trace %))
                    jobs))
     (count (:memory-offers trace))))

(defn guidance-observation
  "Observe the guidance count, with the solver seat taken from the REGISTRATION.

  The seat is a parameter of the measurement predicate, not a location like the
  Lean repo or the Agency endpoint: choosing a different seat silently changes
  what is counted.  So it is pinned before the round like `:lean-revision`, and
  an invocation that names a different seat is a loud mismatch rather than a
  silent retarget."
  [registration trace agency-evidence invoked-seat]
  (let [pinned (:reg/solver-seat registration)]
    (cond
      (not (nonblank-string? pinned))
      {:status :unavailable :reason :missing-solver-seat}

      (and (nonblank-string? invoked-seat) (not= invoked-seat pinned))
      {:status :unavailable :reason :solver-seat-mismatch}

      (not= :ok (:status agency-evidence))
      {:status :unavailable :reason (:reason agency-evidence)}

      :else
      {:status :ok
       :count (guidance-count trace (:jobs agency-evidence) pinned)})))

(defn capability-holds? [capability trace]
  (case capability
    :frame-containment-witnessed
    (or (not (:containment-claimed? trace))
        (and (:containment-probe-recorded? trace)
             (:containment-probe-passed? trace)))
    :created-frame-worked
    (not= (get-in trace [:frame :scaffold-hash])
          (get-in trace [:frame :closing-hash]))
    :unique-disposition
    (or (not (:cycle-closed? trace)) (exactly-one? (:disposition-ids trace)))
    :offer-use-disposition
    (subset? (memory-offer-ids trace) (:memory-disposition-offer-ids trace))
    :need-retrieval
    (subset? (:available-artifact-ids trace) (:need-probe-retrieved-ids trace))
    :promotion-importable
    (subset? (:promoted-artifact-ids trace)
             (:importable-promoted-artifact-ids trace))
    :promotion-need-taggable
    (subset? (:promoted-artifact-ids trace)
             (:need-tagged-promoted-artifact-ids trace))
    :measurement-populated
    (subset? (:required-measurement-fields trace)
             (measurement-fields trace))
    false))

(defn recorded-capability? [capability trace]
  (some #(and (= capability (:capability %)) (:recorded? %)
              (nonblank-string? (:evidence-id %)))
        (:capability-probes trace)))

(defn trace-content-failures [registration trace agency-evidence solver-seat]
  (let [caps (:required-capabilities registration)
        guidance (guidance-observation registration trace agency-evidence
                                       solver-seat)]
    (cond-> []
      (not= (:problem registration) (:problem trace))
      (conj :wrong-problem)
      (= (get-in trace [:frame :scaffold-hash])
         (get-in trace [:frame :closing-hash]))
      (conj :f1-scaffold-identical-frame)
      (and (:cycle-closed? trace) (not (exactly-one? (:disposition-ids trace))))
      (conj :f2-non-unique-disposition)
      (not (subset? (memory-offer-ids trace)
                    (:memory-disposition-offer-ids trace)))
      (conj :f3-undispositioned-offer)
      (not (and (integer? (:stratum-frozen-at trace))
                (integer? (:assigned-at trace))
                (< (:stratum-frozen-at trace) (:assigned-at trace))))
      (conj :f4-stratum-not-frozen-before-assignment)
      (> (count (distinct (attempt-values trace :cycle/regime))) 1)
      (conj :f5-multiple-comparison-regimes)
      (and (= :harness-mode (:cycle/mode trace))
           (not (subset? (surfaced-memory-ids trace)
                         (:cycle/store-snapshot-memory-ids trace))))
      (conj :new-memory-in-harness-round)
      (and (= :store-mode (:cycle/mode trace))
           (> (count (distinct
                      (attempt-values trace :cycle/harness-revision))) 1))
      (conj :harness-changed-in-store-round)
      (and (> (count (distinct
                      (attempt-values trace :cycle/store-revision))) 1)
           (> (count (distinct
                      (attempt-values trace :cycle/harness-revision))) 1))
      (conj :both-channels-varied)
      (not= :ok (:status agency-evidence))
      (conj :direct-channel-evidence-unavailable)
      (and (= :ok (:status agency-evidence))
           (direct-channel-inside-window? trace (:jobs agency-evidence)))
      (conj :direct-channel-used)
      (not= :ok (:status guidance))
      (conj :guidance-evidence-unavailable)
      (and (= :ok (:status guidance))
           (not= (:count guidance)
                 (get-in trace [:measurement :meas/values
                                "attempts or closer hops"])))
      (conj :guidance-measurement-mismatch)
      (or (not (:denominator-declared? trace))
          (:denominator-inferred-from-corpus? trace))
      (conj :f6-denominator-not-preregistered)
      (not (subset? (:available-artifact-ids trace)
                    (:need-probe-retrieved-ids trace)))
      (conj :f7-missed-available-artifact)
      (and (:containment-claimed? trace)
           (not (and (:containment-probe-recorded? trace)
                     (:containment-probe-passed? trace))))
      (conj :f8-unwitnessed-containment)
      (some #(not (capability-holds? % trace)) caps)
      (conj :f9-capability-not-realized)
      (some #(not (recorded-capability? % trace)) caps)
      (conj :f9-capability-probe-missing)
      (not (subset? (:promoted-artifact-ids trace)
                    (:importable-promoted-artifact-ids trace)))
      (conj :promotion-not-importable)
      (not (subset? (:promoted-artifact-ids trace)
                    (:need-tagged-promoted-artifact-ids trace)))
      (conj :promotion-not-need-taggable)
      (not (subset? required-measurement-fields
                    (measurement-fields trace)))
      (conj :measurement-field-claimed-without-value))))

(defn failures
  "Return every distinct diagnostic.  Tests may supply independently observed
  source and Agency evidence explicitly; production callers supply endpoints."
  ([registration trace lean-repo agency-endpoint solver-seat]
   (failures registration trace (lean-source-revision lean-repo)
             (fetch-agency-jobs agency-endpoint) solver-seat :observed))
  ([registration trace actual-lean-revision agency-evidence solver-seat
    _observed]
   (vec (distinct
         (concat (registration-shape-failures registration)
                 (trace-shape-failures trace)
                 (when (map? registration)
                   (registration-content-failures registration
                                                  actual-lean-revision))
                 (when (and (map? registration) (map? trace))
                   (trace-content-failures registration trace
                                           agency-evidence solver-seat)))))))

(defn report [registration trace lean-repo agency-endpoint solver-seat]
  (let [source-revision (lean-source-revision lean-repo)
        agency-evidence (fetch-agency-jobs agency-endpoint)
        guidance (guidance-observation registration trace agency-evidence
                                       solver-seat)
        problems (failures registration trace source-revision
                           agency-evidence solver-seat :observed)]
    {:kind :apm-demonstration-round1-validation
     :lean-source-revision source-revision
     :direct-channel-evidence-status (:status agency-evidence)
     :guidance-evidence-status (:status guidance)
     :guidance-count (:count guidance)
     :failures problems
     :launchable? (empty? problems)}))
