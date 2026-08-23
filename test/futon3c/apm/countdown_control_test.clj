(ns futon3c.apm.countdown-control-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-runner :as runner]
            [futon3c.apm.countdown-control :as sut]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.live-promotion :as live-promotion]
            [futon3c.apm.jit-queue-coordinator :as jit-coordinator]
            [futon3c.apm.problem-projection :as problem-projection]
            [futon3c.apm.problem-queue-supervisor :as problem-queue]
            [futon3c.apm.queued-frame-adapter :as queued-frame-adapter]))

(deftest terminal-lifecycle-actions-have-deterministic-handlers
  (is (= {:ok true :status :certified
          :certificate {:effect :countdown-block-closed :block-id "b1"}}
         (#'sut/drive-live-action! {:kind :close-block :block-id "b1"})))
  (is (= {:ok true :status :certified
          :certificate {:effect :countdown-campaign-closed}}
         (#'sut/drive-live-action! {:kind :close-campaign}))))

(deftest promote-solver-selects-durable-two-seat-adapter
  (let [captured (atom nil)
        action {:kind :scribe-reduce :role :scribe :phase :promote-solver
                :frame-id "f22" :problem-id "p22"}
        inputs {:ok true :action action :contract {} :receipts {}
                :state-path "/tmp/f22-promotion.edn"
                :request {:ledger-digest "ledger"
                          :input-receipt-ids #{"solve" "verify"}}
                :manifest {:apparatus {:artifacts
                                       {:promotion-proctor
                                        {:path "holes/labs/M-apm-demonstration/role-cards/promotion-proctor-v2.md"
                                         :blob "blob"}}}}
                ;; Dispatch authority now existence-checks the card before the
                ;; reviewer can be invoked.
                :unit {:frame/id "f22" :problem/id "p22"}
                :preparation {:seats {:promotion-proctor
                                      {:agent-id "f22-promotion-proctor"}}
                              :seat-policy {:turn-timeout-ms 7200000}}}]
    (with-redefs [sut/live-learning-phase-inputs (constantly inputs)
                  live-promotion/run-live!
                  (fn [opts] (reset! captured opts)
                    {:ok true :status :awaiting-terminal :job-id "scribe-job"})]
      (is (= "scribe-job" (:job-id (sut/drive-live-learning-phase! action))))
      (is (= "f22-promotion-proctor"
             (get-in @captured [:reviewer-request :agent-id])))
      (is (= "blob" (get-in @captured [:reviewer-request :role-card-blob])))
      (is (= 7200000
             (get-in @captured [:reviewer-request :turn-timeout-ms])))
      (is (fn? (:publish-fn @captured))))))

(deftest promotion-resume-reconstructs-authority-when-stage-state-has-no-request
  (let [manifest {:apparatus {:artifacts {:scribe {:path "holes/labs/M-apm-demonstration/role-cards/scribe-v3.md"
                                                   :blob "scribe-blob"}}}}
        contract {:phases {:promote-solver {:kind :scribe-reduce
                                            :role :scribe
                                            :requires #{}}}}
        unit {:frame/id "f22" :problem/id "p22"
              :problem {:blob "problem-blob" :path "Main.lean"}}
        preparation {:workspaces {:student {:workspace/path "/tmp/student"}}
                     :seats {:scribe {:agent-id "f22-scribe"}}}
        action {:kind :scribe-reduce :role :scribe :phase :promote-solver
                :frame-id "f22" :problem-id "p22"}]
    (with-redefs [sut/frame-context
                  (constantly {:ok true :manifest manifest :contract contract
                               :unit unit :preparation preparation})
                  runtime/read-state
                  (fn [path]
                    (when (re-find #"promote-solver" (str path))
                      {:state/type :promotion :stage :deposit :job "scribe-job"}))
                  runtime/http-json
                  (fn [& _] {:agent-id "f22-scribe"
                             :agent {:type "zai" :invoke-ready? true
                                     :metadata {:frame-id "f22"}}})
                  ledger/read-ledger
                  (fn [_] {:projection {:ledger/digest
                                        (apply str (repeat 64 "a"))}})
                  sut/certified-receipts
                  (fn [& _] {:solve {:receipt/id "solve"
                                     :receipt/final-head
                                     "1111111111111111111111111111111111111111"}})]
      (let [result (sut/live-learning-phase-inputs action)]
        (is (:ok result))
        (is (= :promote-solver (get-in result [:request :phase])))
        (is (= "f22-scribe" (get-in result [:request :agent-id])))))))

(deftest ^:slow replacement-registration-starts-at-f19-with-complete-cycle
  (let [body (sut/registration-body)
        units (get-in body [:block-plan 0 :units])]
    (is (= 9 (count units)))
    (is (= "f19" (:frame-id (first units))))
    (is (= "f27" (:frame-id (last units))))
    (is (= 10 (count (:phase-order body))))
    (is (= :preflight (first (:phase-order body))))
    (is (= :close-frame (last (:phase-order body))))
    (is (not-any? #(contains? % :required-receipt-kinds) units)
        "eventual close receipts must not be required at open-frame runtime")))

(deftest m-five-v2-entrypoint-is-fresh-f25-and-self-continuing
  (let [captured (atom nil)
        authority {:agent "codex-10" :session "session-10"
                   :surface "emacs-repl"
                   :control-root "/home/joe/code/futon3c-apm-control"}]
    (with-redefs [sut/set-alight-problem-list!
                  (fn [request] (reset! captured request)
                    {:ok true :status :dry-run})]
      (is (:ok (sut/launch-m-five-v2! authority)))
      (is (= 5 (count (:problems @captured))))
      (is (= "m94A02" (get-in @captured [:problems 0 :problem/id])))
      (is (= "jit-m-five-v2" (:queue-name @captured)))
      (is (= 25 (:frame-number-base @captured)))
      (is (re-find #"launch-m-five-v2!"
                   (get-in @captured [:authority :continuation-payload]))))))

(deftest learning-regime-audit-preserves-v1-and-fails-closed-on-v2-pins
  (let [v1 (:contract (#'sut/inputs))
        v2 (edn/read-string
            (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"))
        manifest (:manifest (#'sut/inputs))]
    (is (= :baseline-v1 (:regime (sut/learning-regime-audit v1 manifest {}))))
    (is (= :learning-regime-incomplete
           (:error/code (sut/learning-regime-audit v2 manifest {:seats {}}))))
    (is (:ok (sut/learning-regime-audit
              v2 (assoc-in manifest [:apparatus :artifacts :promotion-proctor]
                           {:path "promotion-proctor.md" :blob "blob"})
              {:seats {:analyst {:agent-id "analyst-1"}
                       :proctor {:agent-id "f22-proctor"}
                       :promotion-proctor
                       {:agent-id "f22-promotion-proctor"}
                       :scribe {:agent-id "f22-scribe"}}})))))

(deftest baseline-f20-close-does-not-invent-an-analyst-transition
  (with-redefs [sut/frame-context
                (constantly {:ok true :manifest {} :preparation {}
                             :contract (:contract (#'sut/inputs))})]
    (is (= :baseline-v1-no-analyst-transition
           (:status
            (sut/record-analyst-wake!
             "f20" {:receipt/type :frame-close :receipt/result :closed}))))))

(deftest set-alight-drives-live-supervisor-with-exact-continuation
  (let [calls (atom [])
        result
        (sut/set-alight!
         {:agent "codex-10" :session "session-10" :surface "emacs-repl"}
         {:launch-audit-fn #(do (swap! calls conj :audit) {:ok true})
          :inspect-fn #(do (swap! calls conj :inspect)
                           {:ok true :stepper/status :ready
                            :obligation {:obligation/action
                                         {:kind :solve :phase :solve
                                          :frame-id "f19"}}})
          :drive-phase-fn #(do (swap! calls conj [:drive %])
                               {:ok true :status :awaiting-terminal
                                :state {:ticket {:job-id "job-f19-solve"}}})
          :advance-fn (fn [& _] (swap! calls conj :advance) {:ok true})
          :project-fn #(do (swap! calls conj :project) {:ok true})
          :park-fn #(do (swap! calls conj [:park %]) {:ok true})})]
    (is (= :parked (:status result)))
    (is (= "job-f19-solve" (:job-id result)))
    (is (= [:audit :inspect] (take 2 @calls)))
    (is (= ["job-f19-solve"] (get-in (last @calls) [1 :awaiting])))
    (is (re-find #"set-alight!" (get-in (last @calls) [1 :payload])))
    (is (some #{:project} @calls))
    (is (not-any? #{:advance} @calls))))

(deftest set-alight-never-parks-when-launch-audit-fails
  (let [parked? (atom false)
        result (sut/set-alight!
                {:agent "codex-10" :session "wrong" :surface "emacs-repl"}
                {:launch-audit-fn (constantly {:ok false :finding :identity})
                 :inspect-fn (constantly {:ok true})
                 :drive-phase-fn (constantly {:ok true})
                 :advance-fn (constantly {:ok true})
                 :project-fn (constantly {:ok true})
                 :park-fn #(do (reset! parked? true) {:ok true})})]
    (is (= :live-supervisor-launch-audit-failed (:error/code result)))
    (is (false? @parked?))))

(deftest machine-regulator-continuation-does-not-park-an-agent
  (let [capability (var-get
                    (ns-resolve 'futon3c.apm.countdown-control
                                'machine-regulator-capability))
        result
        (sut/set-alight!
         {:regulator-id (str sut/machine-regulator-id ":apm-test")
          :regulator-capability capability :target-frame "f20"}
         {:launch-audit-fn (constantly {:ok true})
          :inspect-fn (constantly
                       {:ok true :stepper/status :ready
                        :obligation {:obligation/action
                                     {:kind :solve :phase :solve
                                      :frame-id "f20"}}})
          :drive-phase-fn (constantly
                           {:ok true :status :awaiting-terminal
                            :state {:ticket {:job-id "job-f20-solve"}}})
          :advance-fn (constantly {:ok true})
          :project-fn (constantly {:ok true})})]
    (is (= :parked (:status result)))
    (is (= "job-f20-solve" (:job-id result)))
    (is (= :machine (get-in result [:park :mode])))
    (is (= ["job-f20-solve"] (get-in result [:park :awaiting])))
    (is (nil? (get-in result [:park :response])))))

(deftest regulator-id-alone-cannot-select-machine-authority
  (let [authorized? (var-get
                     (ns-resolve 'futon3c.apm.countdown-control
                                 'machine-regulator-authorized?))
        capability (var-get
                    (ns-resolve 'futon3c.apm.countdown-control
                                'machine-regulator-capability))]
    (is (false? (authorized? sut/machine-regulator-id nil)))
    (is (false? (authorized? "other" capability)))
    (is (false? (authorized? sut/machine-regulator-id capability)))
    (is (true? (authorized? (str sut/machine-regulator-id ":apm-test")
                            capability)))))

(deftest set-alight-batch-exposes-bounded-ledger-backed-chain
  (let [manifest (:manifest (#'sut/inputs))
        manifest-id (:manifest/id manifest)
        certificate {:campaign/id "apm-countdown-r4"
                     :campaign/manifest-hash manifest-id
                     :campaign/version 5 :ledger/digest "ledger-5"
                     :campaign/permit-usage {}
                     :generated-at "2026-08-21T10:00:00Z"}
        permit (batch/issue
                {:campaign-id "apm-countdown-r4" :manifest-hash manifest-id
                 :start-version 5 :start-ledger-digest "ledger-5"
                 :issuer "joe" :actor "countdown-control" :max-actions 60
                 :allowed-kinds [:preflight]
                 :issued-at "2026-08-21T09:00:00Z"
                 :valid-before "2026-08-22T09:00:00Z"})
        state (atom nil) calls (atom [])
        result
        (sut/set-alight-batch!
         {:start-frame "f20" :end-frame "f25" :permit permit
          :trusted-permit-id (:permit/id permit) :trusted-issuer "joe"}
         {:inspect-fn (fn [] {:ok true :stepper/status :ready
                              :checkpoint {:certificate certificate}
                              :obligation {:obligation/action
                                           {:kind :preflight :frame-id "f20"}}})
          :frame-tick-fn (fn [request] (swap! calls conj request)
                           {:ok true :status :parked :job-id "j20"})
          :cursor-read-fn #(deref state)
          :cursor-persist-fn #(do (reset! state %) {:ok true})})]
    (is (= :parked (:status result)))
    (is (= ["f20" "f21" "f22" "f23" "f24" "f25"]
           (:batch/frames result)))
    (is (= "f20" (:frame-id (first @calls))))
    (is (= (:permit/id permit) (get-in (first @calls) [:permit :permit/id])))))

(defn fake-preparation [manifest unit]
  (let [frame-id (:frame/id unit) problem-id (:problem/id unit)
        body {:preparation/version 2 :campaign/id "apm-countdown-r4"
              :frame/id frame-id :problem/id problem-id
              :manifest/id (:manifest/id manifest)
              :workspaces
              (into {} (map (fn [role]
                              [role {:role role :frame/id frame-id
                                     :problem/id problem-id
                                     :workspace/path (str "/tmp/" frame-id "-" (name role))
                                     :workspace/id (str frame-id "-" (name role) "-workspace")
                                     :branch (str "exp/" frame-id "-" (name role))
                                     :base-revision "1111111111111111111111111111111111111111"}])
                            [:solver :student]))
              :seats
              (into {} (map (fn [[role type]]
                              [role {:agent-id (str frame-id "-" (name role))
                                     :type type}])
                            {:solver :codex :student :zai :guide :claude
                             :proctor :codex :promotion-proctor :codex
                             :scribe :zai}))}]
    (assoc body :preparation/id (machine/ledger-digest [body]))))

(deftest future-frame-contexts-select-exact-units-and-fail-closed-unprovisioned
  (let [manifest (:manifest (#'sut/inputs))]
    (is (= :countdown-frame-not-provisioned
           (:error/code (sut/frame-context "f20"))))
    (doseq [frame-id ["f20" "f21" "f22" "f23" "f24" "f25"]]
      (let [unit (sut/frame-unit manifest frame-id)
            context (sut/frame-context
                     frame-id #(fake-preparation manifest
                                                 (sut/frame-unit manifest %)))]
        (is (:ok context))
        (is (= frame-id (get-in context [:unit :frame/id])))
        (is (= (:problem/id unit) (get-in context [:preparation :problem/id])))))))

(deftest promotion-certified-state-satisfies-generic-phase-handler
  (let [receipt {:receipt/frame-id "f22" :receipt/problem-id "p22"
                 :receipt/id "promotion-receipt"}
        result (with-redefs [runtime/read-state
                             (constantly {:state/type :promotion-certified
                                          :receipt receipt})]
                 (#'sut/certified-handler
                  :scribe-reduce {:frame-id "f22" :problem-id "p22"
                                  :phase :promote-solver}))]
    (is (:ok result))
    (is (= receipt (:certificate result)))))

(deftest live-projection-refreshes-the-certificate-cache-first
  (let [calls (atom [])]
    (with-redefs [runner/checkpoint!
                  (fn [_ stage]
                    (swap! calls conj stage)
                    {:ok false :error/code :deliberate-stop})]
      (is (= :countdown-projection-checkpoint-failed
             (:error/code (#'sut/project-current! "f22"))))
      (is (= [{:checkpoint/stage :live-projection-refresh}] @calls)))))

(deftest certified-one-shot-solve-projects-one-completed-and-no-active-round
  (let [progress (#'sut/solver-projection-progress
                  {:state/type :live-job-certified
                   :rounds []
                   :active {:request {:solver/round 1 :solver/max-rounds 50}}}
                  :student-attempt-1)]
    (is (= 1 (:rounds/completed progress)))
    (is (= 50 (:rounds/max progress)))
    (is (nil? (:round/active progress)))
    (is (nil? (:checkpoint/next progress)))))

(deftest solve-projection-observes-the-active-job-inside-round-state
  (let [active {:state/type :live-job-dispatched
                :request {:solver/round 1}
                :ticket {:job-id "solve-job"}}]
    (is (= active
           (#'sut/projection-phase-job-state
            {:state/type :solver-rounds :rounds [] :active active})))
    (is (= active (#'sut/projection-phase-job-state active)))))

(deftest promotion-projection-observes-the-active-scribe-job
  (is (= {:status :waiting-for-terminal-result
          :role :scribe :agent-id "f27-scribe" :job-id "deposit-job"}
         (#'sut/projection-operation
          "f27" :promote-solver
          {:state/type :promotion :stage :deposit :job "deposit-job"})))
  (is (= :promotion-proctor
         (:role (#'sut/projection-operation
                 "f27" :promote-solver
                 {:state/type :promotion :stage :independent-review
                  :job "review-job"})))))

(deftest checkpoint-projection-does-not-downgrade-current-live-view
  (with-redefs [runtime/read-state
                (constantly {:ledger/digest "ledger-current"})
                problem-projection/project-latest!
                (fn [_] (throw (ex-info "must not overwrite" {})))]
    (is (= :current-ledger-already-published
           (:reason
            (#'sut/projection-sink
             {:certificate {:ledger/digest "ledger-current"
                            :active/frame {:frame-id "f27"
                                           :problem-id "m94A03"}}}))))))

(deftest v2-countdown-policy-is-lean-generated-and-schema-hole-is-explicit
  (binding [sut/contract-path
            "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"]
    (let [loaded (#'sut/inputs)
          contract (:contract loaded)
          emitted (:generated/contract loaded)
          hole (edn/read-string
                (slurp "holes/labs/M-apm-demonstration/hole-generated-receipt-schemas-v1.edn"))]
      (is (= :promote-solver (nth (:phase-order contract) 3)))
      (is (= 50 (get-in contract [:generated/bounds :solver-max-rounds])))
      (is (= "apm-complete-frame-cycle-v2" (:contract-id emitted)))
      (is (map? (:receipt/schemas contract)))
      (is (nil? (:receipt-schemas contract)))
      (is (= :open (:hole/status hole)))
      (is (= 11 (count (:phase-order contract)))))))

(deftest mutated-emitted-contract-fails-before-registration
  (let [source (slurp sut/generated-contract-path)
        temp (java.io.File/createTempFile "apm-contract-mutant" ".json")]
    (spit temp (.replace source "\"solver-max-rounds\":50"
                         "\"solver-max-rounds\":49"))
    (try
      (binding [sut/contract-path
                "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"
                sut/generated-contract-path (.getAbsolutePath temp)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Lean-generated campaign contract rejected"
                              (#'sut/inputs))))
      (finally (.delete temp)))))

(deftest bootstrap-rejects-ledger-registered-under-another-contract
  (binding [sut/contract-path
            "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"]
    (with-redefs [ledger/read-ledger
                  (constantly {:ok true :events [{:event/seq 0}]
                               :projection
                               {:campaign/id :wrong
                                :campaign/manifest-hash "wrong"
                                :campaign/phase-order [:preflight]}})]
      (is (= :countdown-registration-mismatch
             (:error/code (sut/bootstrap!)))))))

(deftest ^:slow qualified-v2-launch-dry-run-dispatches-nothing
  (binding [sut/contract-path
            "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"]
    (let [result (sut/dry-run-v2-launch)]
      (is (:ok result) (pr-str result))
      (is (= [] (:dispatches result)))
      (is (= [] (:historical-state-mutations result)))
      (is (= :f25-frozen (:reference-fixture result)))
      (is (every? true? (vals (:policy-audit result))))
      (is (= :promote-solver
             (nth (get-in result [:registration :phase-order]) 3)))
      (is (= :apm-validated-system-v1
             (get-in result [:qualification :qualification/id]))))))

(deftest stale-qualification-report-blocks-v2-launch
  (let [report (edn/read-string
                (slurp "data/apm-validation/qualification-report-v1.edn"))
        temp (java.io.File/createTempFile "stale-apm-report" ".edn")]
    (spit temp (pr-str (assoc-in report
                                 [:generated-contract :observed-digest]
                                 "stale")))
    (try
      (binding [sut/contract-path
                "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"
                sut/qualification-report-path (.getAbsolutePath temp)]
        (let [result (sut/dry-run-v2-launch)]
          (is (false? (:ok result)))
          (is (some #{:qualification-observed-artifact-stale}
                    (get-in result [:qualification :findings])))
          (is (= [] (:dispatches result)))))
      (finally (.delete temp)))))

(deftest problem-list-entry-does-not-preconstruct-frame-resources
  (let [captured (atom nil)
        problems [{:problem/id "p1" :repository "/repo" :revision "r1"
                   :path "p1.lean" :blob "b1"
                   :classification :non-excluded}
                  {:problem/id "p2" :repository "/repo" :revision "r2"
                   :path "p2.lean" :blob "b2"
                   :classification :non-excluded}]]
    (with-redefs [problem-queue/tick!
                  (fn [options] (reset! captured options)
                    {:ok true :status :frame-prepared})]
      (is (= :frame-prepared
             (:status (sut/set-alight-problem-queue!
                       {:problems problems}
                       {:mint-frame-fn identity}))))
      (is (= ["p1" "p2"]
             (mapv :problem/id (get-in @captured [:plan :problems]))))
      (is (nil? (get-in @captured [:plan :frames])))
      (is (fn? (:state-provider @captured)))
      (is (fn? (:persist-state-fn @captured))))))

(deftest jit-queue-wires-concrete-adapter-and-countdown-supervision
  (let [adapter-config (atom nil)
        tick-options (atom nil)]
    (with-redefs [queued-frame-adapter/live-effects
                  (fn [config]
                    (reset! adapter-config config)
                    {:mint-frame-fn identity})
                  problem-queue/tick!
                  (fn [options]
                    (reset! tick-options options)
                    {:ok true :status :frame-prepared})
                  sut/set-alight! (constantly {:ok true :status :frame-complete})
                  ledger/read-ledger (constantly {:ok true :events []})
                  runtime/read-state (constantly {:preparation/version 2})
                  queued-frame-adapter/terminal-from-ledger
                  (constantly {:ok true :frame/result :closed
                               :terminal-receipt {:receipt/id "terminal"}})]
      (is (= :frame-prepared
             (:status
              (sut/set-alight-problem-queue!
               {:problems [{:problem/id "p1" :repository "/repo" :revision "r"
                            :path "p.lean" :blob "b"
                            :classification :non-excluded}]
                :authority {:agent "codex-10"}}
               {:jit/config {:campaign-root "/campaigns"}}))))
      (is (fn? (:frame-tick-fn @adapter-config)))
      (is (= :closed
             (:frame/result
              ((:frame-tick-fn @adapter-config)
               {:frame/id "f40" :problem/id "p1"}
               {:ledger-path "/tmp/ledger"
                :preparation-path "/tmp/preparation"}))))
      (is (fn? (:mint-frame-fn @tick-options)))
      (is (nil? (:jit/config @tick-options))))))

(deftest list-only-entry-point-supplies-all-concrete-jit-services
  (let [captured (atom nil)
        problem {:problem/id "m-test" :repository "/repo" :revision "r"
                 :path "Main.lean" :blob "b" :classification :non-excluded}]
    (with-redefs [sut/set-alight-problem-queue!
                  (fn [request effects]
                    (reset! captured {:request request :effects effects})
                    {:ok true :status :frame-prepared})
                  runtime/http-json
                  (fn [_ _ payload]
                    {:ok true :http/status 200 :payload payload})
                  ledger/read-ledger
                  (fn [_]
                    {:ok true
                     :projection
                     {:campaign/version 5 :ledger/digest "open-digest"
                      :active/frame {:frame-id "f24" :problem-id "m-test"
                                     :phase :preflight}
                      :active/claim nil}})]
      (is (= :frame-prepared
             (:status (sut/set-alight-problem-list!
                       {:problems [problem]
                        :authority {:agent "codex-10"
                                    :control-root
                                    "/home/joe/code/futon3c-apm-control"
                                    :apparatus-root
                                    "/home/joe/code/futon3c-apm-control"
                                    :campaign-root "/durable/f25"}}))))
      (let [config (get-in @captured [:effects :jit/config])]
        (is (every? fn? (map config [:manifest-fn :open-frame-fn :ledger-fn
                                     :retirement-audit-fn])))
        (is (= 24 (:frame-number-base config)))
        (is (= "/durable/f25" (:campaign-root config)))
        (let [directory (java.nio.file.Files/createTempDirectory
                         "jit-manifest-test"
                         (make-array java.nio.file.attribute.FileAttribute 0))
              manifest-path (str (.resolve directory "manifest.edn"))
              persisted {:manifest/id "frozen"
                         :units [{:frame/id "f24" :problem/id "m-test"
                                  :problem {:revision "r" :path "Main.lean"
                                            :blob "b"}}]}]
          (spit manifest-path (pr-str persisted))
          (is (= persisted
                 ((:manifest-fn config)
                  {:frame/id "f24" :problem/id "m-test" :problem problem}
                  {:manifest-path manifest-path}))))
        (is (= "m-test"
               (:problem-id
                ((:ledger-fn config)
                 {:frame/id "f24" :problem/id "m-test"}
                 {:ledger-path "/unused/ledger.edn"}))))
        (is (= {:ok true :already-open? true
                :ledger/version 5 :ledger/digest "open-digest"}
               ((:open-frame-fn config)
                {:frame/id "f24" :problem/id "m-test"} nil
                {:ledger-path "/unused/ledger.edn"})))
        (is (string? (get-in @captured
                             [:request :authority :continuation-payload])))
        (is (= "/durable/f25/queue-state.edn"
               (get-in @captured
                       [:request :campaign-config :problem-queue-state-path])))
        (is (= [problem] (get-in @captured [:request :problems])))))))

(deftest autonomous-entry-registers-machine-authority-without-agent-session
  (let [captured (atom nil)
        problem {:problem/id "p1" :repository "/repo" :revision "r"
                 :path "Main.lean" :blob "b" :classification :non-excluded}]
    (with-redefs [jit-coordinator/start!
                  (fn [options] (reset! captured options)
                    {:ok true :status :started})]
      (is (:ok (sut/start-autonomous-problem-list!
                {:problems [problem] :queue-name "q" :frame-number-base 26
                 :coordinator-registry-path "/tmp/registry.edn"
                 :coordinator-state-path "/tmp/coordinator.edn"
                 :authority {:agent "codex-10" :session "session-1"
                             :surface "emacs-repl"
                             :control-root "/home/joe/code/futon3c"
                             :campaign-root "/campaign"}})))
      (is (= "jit-queue:q" (:coordinator-id @captured)))
      (is (= "/tmp/registry.edn" (:registry-path @captured)))
      (is (= "/tmp/coordinator.edn" (:state-path @captured)))
      (is (nil? (get-in @captured [:launch :authority :agent])))
      (is (nil? (get-in @captured [:launch :authority :session])))
      (is (= "/campaign" (get-in @captured [:launch :authority
                                             :campaign-root]))))))

(deftest m-five-v2-autonomous-entry-does-not-run-an-operator-step
  (let [captured (atom nil)
        authority {:control-root "/home/joe/code/futon3c"
                   :campaign-root "/campaign"}]
    (with-redefs [clojure.core/slurp
                  (fn [_] (pr-str {:problems [{:problem/id "p1"}]}))
                  sut/start-autonomous-problem-list!
                  (fn [request] (reset! captured request)
                    {:ok true :status :started})]
      (is (= :started (:status (sut/launch-m-five-v2-autonomous! authority))))
      (is (= "jit-m-five-v2" (:queue-name @captured)))
      (is (= 25 (:frame-number-base @captured)))
      (is (= [{:problem/id "p1"}] (:problems @captured))))))
