(ns futon3c.apm.preregistration-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.preregistration :as prereg])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net InetSocketAddress]))

(def problem
  {:problem-id "round-1-problem-not-yet-selected"
   :difficulty-stratum "caller-supplied"
   :regime "caller-supplied"
   :locked-lemma-exposure ["caller-supplied"]})

(def registration
  {:kind :apm-demonstration-round1-registration
   :schema 1
   :lean-registration prereg/required-lean-registration
   :lean-source prereg/required-lean-source
   :lean-revision prereg/required-lean-revision
   :modules prereg/required-modules
   :structural-invariants prereg/required-structural-invariants
   :runtime-invariants prereg/required-runtime-invariants
   :problem problem
   :variation {:kind :controlled :endpoint "caller-supplied"}
   :claim :descriptive
   :arms [{:name "one problem, one-shot measurement"
           :neutral? false :axes [] :role :treatment}]
   :replication-stage :pilot
   :pilot-units [problem]
   :confirmation-units []
   :estimated-cost 1
   :budget-cap 1
   :teardown-deadline nil
   :stop-rules [:caller-supplied]
   :decision-rule {:id :caller-supplied :outcomes [:caller-supplied]}
   :required-capabilities prereg/required-capabilities
   :required-measurement-fields prereg/required-measurement-fields
   :reg/role-cards {:solver prereg/required-lean-revision
                    :adjudicator prereg/required-lean-revision}
   :reg/solver-seat "codex-4"})

(def revision-a "1111111111111111111111111111111111111111")
(def revision-b "2222222222222222222222222222222222222222")

(deftest guidance-regime-is-optional-and-validated-when-present
  (is (not-any? #(= :invalid-guidance-regime (:finding %))
                (prereg/registration-shape-failures registration)))
  (is (not-any? #(= :invalid-guidance-regime (:finding %))
                (prereg/registration-shape-failures
                 (assoc registration :reg/guidance-regime
                        #{:suggest :challenge}))))
  (is (some #(= :invalid-guidance-regime (:finding %))
            (prereg/registration-shape-failures
             (assoc registration :reg/guidance-regime #{:suggest :unknown})))))

(deftest solver-config-is-optional-and-shape-validated-when-present
  (let [pin {:model "gpt-5.6-sol" :reasoning-effort "high"}]
    (is (not-any? #(= :invalid-solver-config (:finding %))
                  (prereg/registration-shape-failures registration)))
    (is (not-any? #(= :invalid-solver-config (:finding %))
                  (prereg/registration-shape-failures
                   (assoc registration :reg/solver-config pin))))
    (doseq [malformed [{:model "gpt-5.6-sol"}
                       {:model "" :reasoning-effort "high"}
                       {:model "gpt-5.6-sol" :reasoning-effort "high"
                        :unwitnessed true}
                       "default"]]
      (is (some #(= :invalid-solver-config (:finding %))
                (prereg/registration-shape-failures
                 (assoc registration :reg/solver-config malformed)))
          (pr-str malformed)))))

(deftest student-runner-budget-is-optional-and-shape-validated-when-present
  (is (not-any? #(= :invalid-student-runner-budget (:finding %))
                (prereg/registration-shape-failures registration)))
  (is (not-any? #(= :invalid-student-runner-budget (:finding %))
                (prereg/registration-shape-failures
                 (assoc registration :reg/student-runner-budget
                        {:wall-clock-minutes 45}))))
  (doseq [malformed [{:wall-clock-minutes 0}
                     {:wall-clock-minutes 60.0}
                     {:wall-clock-minutes 60 :extra true}
                     "60"]]
    (is (some #(= :invalid-student-runner-budget (:finding %))
              (prereg/registration-shape-failures
               (assoc registration :reg/student-runner-budget malformed)))
        (pr-str malformed))))

(def attempts
  [{:cycle/regime "regime/a"
    :cycle/store-revision revision-a
    :cycle/harness-revision revision-a
    :cycle/environment-checkout "/frames/solver"
    :cycle/environment-revision revision-a
    :cycle/runner-freshness true}
   {:cycle/regime "regime/a"
    :cycle/store-revision revision-a
    :cycle/harness-revision revision-a
    :cycle/environment-checkout "/frames/student"
    :cycle/environment-revision revision-a
    :cycle/runner-freshness true}])

(def trace
  {:problem problem
   :frame {:scaffold-hash "scaffold" :closing-hash "closing"}
   :cycle-closed? true
   :disposition-ids ["terminal"]
   :memory-offers [{:offer/id "offer" :offer/memory-id "memory/known"}]
   :memory-disposition-offer-ids ["offer"]
   :stratum-frozen-at 1
   :assigned-at 2
   :cycle/attempts attempts
   :cycle/mode :harness-mode
   :cycle/deposit-state :n/a
   :cycle/paired-with nil
   :cycle/store-snapshot-id "snapshot/round-open"
   :cycle/store-snapshot-memory-ids ["memory/known"]
   :cycle/window {:opened-at "2026-08-14T12:00:00Z"
                  :closed-at "2026-08-14T13:00:00Z"}
   :denominator-declared? true
   :denominator-inferred-from-corpus? false
   :available-artifact-ids ["artifact"]
   :need-probe-retrieved-ids ["artifact"]
   :containment-claimed? true
   :containment-probe-recorded? true
   :containment-probe-passed? true
   :capability-probes
   (mapv (fn [capability]
           {:capability capability
            :evidence-id (str "evidence/" (name capability))
            :recorded? true})
         prereg/required-capabilities)
   :required-measurement-fields prereg/required-measurement-fields
   :measurement
   {:meas/values (assoc (into {} (map (fn [field] [field :observed])
                                      prereg/required-measurement-fields))
                        "attempts or closer hops" 0)
    :meas/unset {}}
   :promoted-artifact-ids ["promotion"]
   :importable-promoted-artifact-ids ["promotion"]
   :need-tagged-promoted-artifact-ids ["promotion"]})

(def opening-job
  {:agent-id "codex-4" :caller "cycle-machine"
   :created-at "2026-08-14T12:00:00Z"})

(defn checked [registration trace]
  (prereg/failures registration trace prereg/required-lean-revision
                   {:status :ok :jobs [opening-job]} "codex-4" :observed))

(deftest aligned-positive-witness-is-launchable
  (is (empty? (checked registration trace))))

(deftest attempt-environment-fields-are-required-and-typed
  (let [attempt (first attempts)]
    (is (false? (prereg/attempt? (dissoc attempt :cycle/environment-checkout))))
    (is (false? (prereg/attempt? (dissoc attempt :cycle/environment-revision))))
    (is (false? (prereg/attempt?
                 (assoc attempt :cycle/environment-revision "not-a-sha"))))
    (is (true? (prereg/attempt? attempt)))))

(deftest structural-errors-are-not-misreported-as-content-errors
  (let [failures (checked (dissoc registration :problem) trace)]
    (is (some #{:registration-missing-required-key} failures))
    (is (some #{:malformed-problem} failures))))

(deftest carded-roles-require-staffed-and-separated-seats
  (let [role-cards (zipmap [:solver :guide :proctor :scribe :student]
                           (repeat prereg/required-lean-revision))
        staffed (assoc registration
                       :reg/role-cards role-cards
                       :reg/guide-seat "claude-7"
                       :reg/proctor-seat "ams-codex-2"
                       :reg/scribe-seat "claude-8"
                       :reg/student-seat "zai-1")]
    (testing "one role-bearing finding identifies a missing proctor seat"
      (is (some #(= {:finding :unstaffed-carded-seat
                     :role :proctor
                     :seat-key :reg/proctor-seat}
                    %)
                (prereg/registration-shape-failures
                 (dissoc staffed :reg/proctor-seat)))))
    (testing "guide and proctor seats must be distinct"
      (is (some #{:guide-proctor-not-separated}
                (prereg/registration-shape-failures
                 (assoc staffed :reg/proctor-seat "claude-7")))))
    (testing "fully staffed distinct carded roles add no failures"
      (is (empty? (prereg/registration-shape-failures staffed))))
    (testing "a non-map role-card value degrades to findings, never throws"
      (is (some #{:malformed-role-cards}
                (prereg/registration-shape-failures
                 (assoc staffed :reg/role-cards "garbage")))))
    (testing "absence of a role-card map adds no seat findings"
      (let [failures (prereg/registration-shape-failures
                      (dissoc staffed :reg/role-cards))]
        (is (not-any? #(and (map? %)
                            (= :unstaffed-carded-seat (:finding %)))
                      failures))
        (is (not (some #{:guide-proctor-not-separated} failures)))))))

(deftest every-runtime-invariant-has-a-named-failure
  (doseq [[expected bad-trace]
          [[:f2-non-unique-disposition (assoc trace :disposition-ids [])]
           [:f3-undispositioned-offer
            (assoc trace :memory-disposition-offer-ids [])]
           [:f4-stratum-not-frozen-before-assignment
            (assoc trace :assigned-at 1)]
           [:f5-multiple-comparison-regimes
            (assoc-in trace [:cycle/attempts 1 :cycle/regime] "regime/b")]
           [:f6-denominator-not-preregistered
            (assoc trace :denominator-declared? false)]
           [:f7-missed-available-artifact
            (assoc trace :need-probe-retrieved-ids [])]
           [:f8-unwitnessed-containment
            (assoc trace :containment-probe-recorded? false)]
           [:f9-capability-probe-missing
            (update trace :capability-probes pop)]]]
    (testing (name expected)
      (is (some #{expected} (checked registration bad-trace))))))

(deftest f1-and-the-lean-pin-fail-loudly
  (is (some #{:f1-scaffold-identical-frame}
            (checked registration
                     (assoc-in trace [:frame :closing-hash] "scaffold"))))
  (is (some #{:stale-lean-revision}
            (prereg/failures registration trace
                            "0000000000000000000000000000000000000000"
                            {:status :ok :jobs [opening-job]} "codex-4"
                            :observed))))

(deftest all-failures-are-returned-together
  (let [failures (checked (assoc registration :estimated-cost 2)
                          (-> trace
                              (assoc :denominator-declared? false)
                              (assoc-in [:cycle/attempts 1 :cycle/regime]
                                        "regime/b")))]
    (is (every? (set failures)
                [:over-budget :f5-multiple-comparison-regimes
                 :f6-denominator-not-preregistered]))))

(deftest claimed-measurement-fields-require-values
  (let [bad-trace (assoc trace :measurement
                         {:meas/values {} :meas/unset {}})]
    (is (some #{:measurement-field-claimed-without-value}
              (checked registration bad-trace)))))

(deftest declared-unset-measurement-with-reason-is-valid
  (let [field (first prereg/required-measurement-fields)
        honest-trace (-> trace
                         (update-in [:measurement :meas/values] dissoc field)
                         (assoc-in [:measurement :meas/unset field]
                                   "deferred to pilot observation"))]
    (is (empty? (checked registration honest-trace)))
    (is (= "deferred to pilot observation"
           (get-in honest-trace [:measurement :meas/unset field])))))

(deftest harness-round-refuses-memory-outside-round-open-snapshot
  (let [bad-trace (assoc-in trace [:memory-offers 0 :offer/memory-id]
                            "memory/created-during-round")]
    (is (some #{:new-memory-in-harness-round}
              (checked registration bad-trace)))))

(deftest store-round-refuses-changing-harness-revision
  (let [bad-trace (-> trace
                      (assoc :cycle/mode :store-mode)
                      (assoc-in [:cycle/attempts 1 :cycle/harness-revision]
                                revision-b))]
    (is (some #{:harness-changed-in-store-round}
              (checked registration bad-trace)))))

(defn with-job-server [status body f]
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)]
    (.createContext
     server "/jobs"
     (reify HttpHandler
       (handle [_ exchange]
         (let [bytes (.getBytes body "UTF-8")]
           (.sendResponseHeaders exchange status (count bytes))
           (with-open [output (.getResponseBody exchange)]
             (.write output bytes))))))
    (.start server)
    (try
      (f (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/jobs"))
      (finally (.stop server 0)))))

(deftest agency-log-refuses-direct-claude-to-zai-channel
  (with-job-server
    200
    "{\"jobs\":[{\"caller\":\"claude-2\",\"agent-id\":\"zai-1\",\"created-at\":\"2026-08-14T12:30:00Z\"}]}"
    (fn [endpoint]
      (let [evidence (prereg/fetch-agency-jobs endpoint)
            failures (prereg/failures registration trace
                                      prereg/required-lean-revision
                                      evidence "codex-4" :observed)]
        (is (= :ok (:status evidence)))
        (is (some #{:direct-channel-used} failures))))))

(deftest unavailable-agency-log-is-not-clean-evidence
  (with-job-server
    503 "{}"
    (fn [endpoint]
      (let [evidence (prereg/fetch-agency-jobs endpoint)
            failures (prereg/failures registration trace
                                      prereg/required-lean-revision
                                      evidence "codex-4" :observed)]
        (is (= :unavailable (:status evidence)))
        (is (some #{:direct-channel-evidence-unavailable} failures))
        (is (some #{:guidance-evidence-unavailable} failures))
        (is (nil? (:count (prereg/guidance-observation
                           registration trace evidence "codex-4"))))
        (is (not (some #{:direct-channel-used} failures)))))))

(deftest guidance-counts-recipient-and-window-not-claimed-caller
  (let [jobs [opening-job
              ;; Both are guidance despite spoofed/missing caller.
              {:agent-id "codex-4" :caller "not-the-guide"
               :created-at "2026-08-14T12:10:00Z"}
              {:agent-id "codex-4"
               :created-at "2026-08-14T12:20:00Z"}
              ;; Excluded: outside window and wrong recipient.
              {:agent-id "codex-4" :caller "claude-guide"
               :created-at "2026-08-14T14:00:00Z"}
              {:agent-id "zai-1" :caller "claude-guide"
               :created-at "2026-08-14T12:30:00Z"}]]
    (is (= 2 (prereg/guidance-count trace jobs "codex-4")))))

(deftest solver-seat-comes-from-the-registration-not-the-invocation
  ;; The seat is a parameter of the measurement predicate, not a location:
  ;; naming a different seat at validation time would silently change the
  ;; number. An invocation that disagrees with the pin is a loud failure.
  (let [evidence {:status :ok :jobs [opening-job]}]
    (is (= :ok (:status (prereg/guidance-observation
                         registration trace evidence "codex-4"))))
    (is (= :solver-seat-mismatch
           (:reason (prereg/guidance-observation
                     registration trace evidence "codex-9"))))
    (is (= :missing-solver-seat
           (:reason (prereg/guidance-observation
                     (dissoc registration :reg/solver-seat)
                     trace evidence "codex-4"))))))

(deftest machine-opening-dispatch-is-not-guidance
  (is (zero? (prereg/guidance-count trace [opening-job] "codex-4"))))

(deftest guidance-count-uses-openings-not-cascade-offer-cardinality
  (let [offer (fn [i]
                {:offer/id (str "offer/dispatch/" i)
                 :offer/memory-id (str "memory/" i)
                 :offer/route (if (zero? i) :leaf :why-hop)
                 :offer/hops (if (zero? i) 0 1)})
        cascade-trace (assoc trace :solver-dispatches [{:job-id "dispatch"}])]
    (is (zero? (prereg/guidance-count
                (assoc cascade-trace :memory-offers [(offer 0)])
                [opening-job] "codex-4"))
        "one dispatch with one offer is one machine opening")
    (is (zero? (prereg/guidance-count
                (assoc cascade-trace :memory-offers (mapv offer (range 102)))
                [opening-job] "codex-4"))
        "one dispatch remains one opening when its cascade offers 102 memories")
    (is (zero? (prereg/guidance-count (dissoc trace :solver-dispatches)
                                      [opening-job] "codex-4"))
        "a cascade-off legacy trace reproduces today's zero count")))

(deftest stored-guidance-measurement-must-match-agency-derived-count
  (let [jobs [opening-job
              {:agent-id "codex-4" :caller "spoofed"
               :created-at "2026-08-14T12:30:00Z"}]
        failures (prereg/failures
                  registration trace prereg/required-lean-revision
                  {:status :ok :jobs jobs} "codex-4" :observed)]
    (is (some #{:guidance-measurement-mismatch} failures))))

(deftest cycle-refuses-both-revision-sequences-changing
  (let [bad-trace (-> trace
                      (assoc-in [:cycle/attempts 1 :cycle/store-revision]
                                revision-b)
                      (assoc-in [:cycle/attempts 1 :cycle/harness-revision]
                                revision-b))]
    (is (some #{:both-channels-varied}
              (checked registration bad-trace)))))
