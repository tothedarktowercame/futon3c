(ns futon3c.apm.countdown-control-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-runner :as runner]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.countdown-control :as sut]
            [futon3c.apm.countdown-pre-admission :as admission]
            [futon3c.apm.countdown-manifest :as countdown-manifest]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.live-promotion :as live-promotion]
            [futon3c.apm.live-supervisor :as live-supervisor]
            [futon3c.apm.jit-queue-coordinator :as jit-coordinator]
            [futon3c.apm.problem-projection :as problem-projection]
            [futon3c.apm.problem-queue-supervisor :as problem-queue]
            [futon3c.apm.queued-frame-adapter :as queued-frame-adapter]))

(def campaign-priors-fixture
  (-> "test/resources/apm-regressions/campaign-priors-legacy-v1.edn"
      slurp edn/read-string))

(deftest jit-one-off-registration-does-not-read-countdown-registration
  (let [manifest {:manifest/id "jit-manifest"
                  :manifest/scope :one-off
                  :campaign/id "jit-frame"
                  :block/id "jit-block"
                  :units [{:frame/id "f51" :problem/id "p51"}]}
        contract {:phase-order [:preflight]
                  :phases {:preflight {:kind :proof :role :solver :ordinal 1}}
                  :terminal-policy {:closure-policy-version "v1"}}
        appended (atom nil)]
    (with-redefs [sut/registration-body
                  (fn [] (throw (ex-info "F19 registration reached" {})))
                  countdown-manifest/validate (constantly {:valid? true})
                  admission/validate (constantly {:ok true :registration/hash "r"})
                  ledger/read-ledger
                  (constantly {:ok true :events []
                               :projection (machine/projection [])})
                  ledger/compare-and-append!
                  (fn [_ _ _ event]
                    (reset! appended event)
                    {:ok true :status :appended})]
      (is (:ok (sut/bootstrap-one-off! manifest contract)))
      (is (= "jit-frame" (:event/campaign-id @appended)))
      (is (= "jit-manifest"
             (get-in @appended [:event/body :manifest-hash]))))))

(deftest jit-frame-tick-and-launch-audit-never-read-countdown-manifest
  (let [missing "/tmp/countdown-manifest-must-not-be-read.edn"
        manifest {:manifest/id "jit-f51-manifest"
                  :manifest/scope :one-off
                  :campaign/id "jit-f51"
                  :block/id "jit-f51-block"
                  :units [{:frame/id "f51" :problem/id "p51"}]}
        contract {:contract/id :apm-complete-frame-cycle-v2
                  :phase-order [:preflight]}
        observed (atom nil)
        effects (queued-frame-adapter/live-effects
                 {:campaign-root "/tmp/jit-no-countdown-manifest"
                  :manifest-fn (fn [_ _] manifest)
                  :contract contract
                  :frame-tick-fn
                  (fn [_ frame-config]
                    (binding [sut/manifest-path missing]
                      (sut/set-alight!
                       {:target-frame "f51" :campaign-config frame-config}
                       {:launch-audit-fn
                        (fn []
                          (reset! observed (#'sut/inputs))
                          {:ok true})
                        :inspect-fn
                        (constantly {:ok true :stepper/status :complete})
                        :project-fn (constantly {:ok true})
                        :park-fn (constantly {:ok true})})))})]
    (is (not (.exists (java.io.File. missing))))
    (is (:ok ((:frame-tick-fn effects)
              {:frame/id "f51" :campaign/id "jit-f51"})))
    (is (= manifest (:manifest @observed)))
    (is (= contract (:contract @observed)))))

(deftest supplied-jit-boot-authority-reaches-default-supervisor-advance
  (let [booted {:ok true :status :already-registered
                :projection {:campaign/id "jit-f51"}}
        observed (atom nil)]
    (with-redefs [live-supervisor/tick!
                  (fn [{:keys [advance-fn]}]
                    (advance-fn :solve {:certificate/id "phase-certificate"}))
                  sut/advance!
                  (fn [kind batch-authority boot]
                    (reset! observed [kind batch-authority boot])
                    {:ok true :status :advanced})
                  sut/bootstrap!
                  (fn []
                    (throw (ex-info "F19 bootstrap reached" {})))]
      (is (:ok (sut/set-alight! {} {:booted booted})))
      (is (= [:solve nil booted] @observed)))))

(deftest campaign-priors-use-legacy-ledgers-final-snapshots-and-declared-lineage
  (let [{:keys [current-campaign lineage queues ledgers snapshots expected]}
        campaign-priors-fixture
        queue-path (str "/fixture/campaigns/" current-campaign "/queue-state.edn")
        read-state
        (fn [path]
          (get queues (-> path .getParent .getFileName str)))
        read-ledger
        (fn [path]
          (let [path (str path)
                events (some (fn [[[campaign frame] events]]
                               (when (str/ends-with?
                                      path (str campaign "-" frame "/ledger.edn"))
                                 events))
                             ledgers)]
            {:ok true :events events}))
        read-edn
        (fn [path]
          (let [path (str path)]
            (if (str/ends-with? path "/lineage.edn")
              lineage
              (get snapshots path))))
        legacy-events (get ledgers ["prior-v1" "f10"])
        result (sut/campaign-prior-memories
                queue-path {:read-state-fn read-state
                            :read-ledger-fn read-ledger
                            :read-edn-fn read-edn})]
    (is (= :valid (:projection/status (machine/projection legacy-events)))
        "the pre-closure-policy ledger remains replayable")
    (is (:ok result) (pr-str result))
    (is (= (:lineage expected) (:lineage result)))
    (is (= (:memory-ids expected)
           (mapv :memory-id (:candidates result)))
        "queue order and the final snapshot receipt determine candidate order")
    (is (= (:dropped expected) (:dropped result))
        "an unreadable snapshot drops only its frame with a stated finding")
    (is (not-any? #(= "rogue" (:memory-id %)) (:candidates result))
        "campaigns outside declared lineage are never discovered")))

(deftest jit-gate-uses-captured-one-off-manifest
  (let [one-off {:manifest/id "f46-manifest" :manifest/scope :one-off
                 :units [{:frame/id "f46" :problem/id "a96J08"}]}
        countdown {:manifest/id "countdown-f18-f27"
                   :units [{:frame/id "f18"}]}
        contract {:contract/id :apm-complete-frame-cycle-v2}
        observed (atom nil)]
    (with-redefs [admission/validate
                  (fn [request]
                    (reset! observed request)
                    {:checks {:unit-present? true
                              :unit-non-topology? true
                              :unit-apparatus-matches? true}})]
      (let [provider (partial #'sut/gate-provider
                              {:manifest one-off :contract contract})
            [gate] (provider {:obligation
                              {:obligation/action {:frame-id "f46"}}})]
        (is (= :pass (:gate/status gate)))
        (is (= one-off (:countdown-manifest @observed)))
        (is (not= countdown (:countdown-manifest @observed)))
        (is (= "f46" (:frame-id @observed)))))))

(defn- valid-combined-trace-receipt
  "A receipt in the shape campaign-trace/valid-combined-trace-receipt? accepts."
  []
  (let [trace-body {"schemaVersion" 1 "traceKind" "countdown-close-test"}
        digest (campaign-trace/combined-trace-digest trace-body)]
    {:trace/combined trace-body
     :trace/digest digest
     :trace/projected-from-durable-state? true
     :trace/observation-kinds (mapv :kind (campaign-trace/observation-schemas))
     :trace/checker-receipt {:checker/status :accepted :trace/digest digest}}))

(deftest terminal-lifecycle-actions-have-deterministic-handlers
  (is (= {:ok true :status :certified
          :certificate {:effect :countdown-block-closed :block-id "b1"}}
         (#'sut/drive-live-action! {:kind :close-block :block-id "b1"}))))

;; Campaign closure is gated on a combined-trace receipt (dd2d5abd). Both
;; directions are asserted here: without a receipt the campaign must not close,
;; and the refusal must name why rather than failing generically.
(deftest close-campaign-requires-a-combined-trace-receipt
  (with-redefs [ledger/read-ledger
                (constantly {:projection {:campaign/frames {}}})]
    (is (= {:ok false :error/code :countdown-campaign-combined-trace-unavailable}
           (#'sut/drive-live-action! {:kind :close-campaign}))))
  (with-redefs [ledger/read-ledger
                (constantly {:projection
                             {:campaign/frames
                              {"f1" {:close-certificate
                                     {:trace/combined {"schemaVersion" 1}
                                      :trace/digest "not-a-real-digest"}}}}})]
    (is (= :countdown-campaign-combined-trace-unavailable
           (:error/code (#'sut/drive-live-action! {:kind :close-campaign})))
        "an invalid receipt is not a receipt")))

(deftest close-campaign-certifies-with-a-valid-receipt
  (let [receipt (valid-combined-trace-receipt)]
    (with-redefs [ledger/read-ledger
                  (constantly {:projection
                               {:campaign/frames
                                {"f1" {:close-certificate receipt}}}})]
      (let [result (#'sut/drive-live-action! {:kind :close-campaign})]
        (is (:ok result))
        (is (= :certified (:status result)))
        (is (= :countdown-campaign-closed (:effect (:certificate result))))
        (is (= (:trace/digest receipt) (:trace/digest (:certificate result)))
            "the certificate carries the receipt's own trace digest")))))

(deftest role-turn-timeout-distinguishes-student-attempts
  (let [contract {:generated/bounds {:seat-turn-timeout-ms 3600000
                                     :student-turn-timeout-ms 1800000}}]
    (is (= 1800000 (#'sut/role-turn-timeout-ms contract :student)))
    (is (= 3600000 (#'sut/role-turn-timeout-ms contract :solver)))
    (is (= 3600000 (#'sut/role-turn-timeout-ms contract :guide)))))

(deftest promote-solver-selects-durable-two-seat-adapter
  (let [captured (atom nil)
        action {:kind :scribe-reduce :role :scribe :phase :promote-solver
                :frame-id "f22" :problem-id "p22"}
        inputs {:ok true :action action :contract {} :receipts {}
                :state-path "/tmp/f22-promotion.edn"
                :request {:role :promotion-proctor
                          :ledger-digest "ledger"
                          :input-receipt-ids #{"solve" "verify"}}
                :fresh-request {:role :scribe
                                :ledger-digest "ledger"
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
      (is (= :scribe (get-in @captured [:deposit-request :role])))
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

(deftest durable-close-records-fingerprint-audit-without-gating-on-its-verdict
  (let [audit-call (atom nil)
        audit-result {:ok false :error/code :fingerprint-audit-command-failed}]
    (with-redefs [sut/record-analyst-wake!
                  (fn [frame receipt]
                    {:ok true :frame frame :receipt receipt})]
      (let [result (sut/record-close-observations!
                    {:ok true :effect :closed} "f58" {:receipt/id "close-58"}
                    (fn [request]
                      (reset! audit-call request)
                      audit-result))]
        (is (= {:ok true :effect :closed}
               (select-keys result [:ok :effect])))
        (is (= audit-result (:fingerprint-audit result)))
        (is (= "f58" (get-in result [:analyst-wake :frame])))
        (is (string? (:state-directory @audit-call)))))))

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
                            {:solver :codex :student :zai :guide :zai
                             :proctor :codex :promotion-proctor :codex
                             :scribe :codex :zai-scribe :zai}))}]
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

(deftest f30-shaped-launch-replay-accepts-certified-retired-solver
  (let [solver {:workspace/id "f30-solver" :workspace/path "/missing/f30-solver"
                :frame/id "f30" :problem/id "a01J06" :role :solver
                :repository/path "/repo" :branch "exp/f30-solver"
                :base-revision "base"}
        result (#'sut/validate-live-workspaces
                {:workspaces {:solver solver}}
                {:solver "terminal-head"}
                (fn [workspace terminal-head]
                  (is (= solver workspace))
                  (is (= "terminal-head" terminal-head))
                  {:ok true :status :already-retired
                   :receipt {:receipt/type :workspace-retired}}))]
    (is (:ok result) (pr-str result))))

(deftest missing-workspace-without-retirement-receipt-still-fails-closed
  (let [result (#'sut/validate-live-workspaces
                {:workspaces
                 {:solver {:workspace/path "/missing/f30-solver"
                           :role :solver :base-revision "base"}}}
                {:solver "terminal-head"}
                (fn [_ _] {:ok true :status :not-retired}))]
    (is (= :countdown-frame-workspace-invalid (:error/code result)))
    (is (some #(= :workspace-path-missing (:finding %))
              (:findings result)))))

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

(deftest end-reduction-projection-observes-the-zai-scribe-job
  (is (= {:status :waiting-for-terminal-result
          :role :zai-scribe
          :agent-id "f32-zai-scribe"
          :job-id "zai-deposit-job"}
         (#'sut/projection-operation
          "f32" :scribe-reduce
          {:state/type :promotion :stage :deposit :job "zai-deposit-job"}))))

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

(deftest v2-countdown-policy-and-receipt-schemas-are-lean-generated
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
      (is (= (set (map keyword
                       (get-in emitted
                               [:receipt-schemas :student-attempt :required])))
             (get-in contract [:receipt/schemas :student-attempt :required])))
      (is (= :closed (:hole/status hole)))
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

(deftest ^:slow v2-manifest-qualifies-under-real-lean
  ;; The one test that provisions the qualification worktrees and runs
  ;; `lake env lean` on all ten pinned problems (~25 s). Everything else
  ;; stubs `qualify-unit` (see futon3c.apm.test-support). Excluded by
  ;; default; run with scripts/apm-test-slow.sh.
  (let [manifest (:manifest (#'sut/inputs))
        result (countdown-manifest/validate manifest)]
    (is (:valid? result) (pr-str (:findings result)))
    (is (= 10 (count (:eligibility-observations result))))
    (is (every? :valid? (:eligibility-observations result)))))

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

(deftest problem-list-threads-memory-cascade-arm-into-jit-adapter
  (let [adapter-config (atom nil)
        cascade {:enabled? true :routes [:sibling] :cap 100}
        problem {:problem/id "m-test" :repository "/repo" :revision "r"
                 :path "Main.lean" :blob "b" :classification :non-excluded}]
    (with-redefs [queued-frame-adapter/live-effects
                  (fn [config]
                    (reset! adapter-config config)
                    {:mint-frame-fn identity})
                  problem-queue/tick!
                  (constantly {:ok true :status :batch-complete})]
      (sut/set-alight-problem-list!
       {:problems [problem] :memory-cascade cascade
        :authority {:agent "codex-21" :control-root "/home/joe/code/futon3c"}
        :autonomous? true})
      (is (= cascade (:memory-cascade @adapter-config))))))

(deftest campaign-conditions-file-is-read-per-tick-and-threaded-into-mint
  (let [dir (java.nio.file.Files/createTempDirectory
             "campaign-conditions-" (make-array java.nio.file.attribute.FileAttribute 0))
        root (str dir)
        live {:id "C-1" :at "2026-08-26T18:40Z" :by "claude-19" :kind :arm
              :note "memory-cascade arm on" :head "2f8d6696"}
        withdrawn (assoc live :id "C-0" :until "2026-08-26T18:41Z")
        adapter-config (atom nil)
        problem {:problem/id "m-test" :repository "/repo" :revision "r"
                 :path "Main.lean" :blob "b" :classification :non-excluded}]
    (is (= [] (sut/campaign-conditions root)) "no file => nothing registered")
    (spit (str root "/conditions.edn") (pr-str [withdrawn live]))
    (is (= [live] (sut/campaign-conditions root))
        "entries with :until are no longer in force")
    (with-redefs [queued-frame-adapter/live-effects
                  (fn [config]
                    (reset! adapter-config config)
                    {:mint-frame-fn identity})
                  problem-queue/tick!
                  (constantly {:ok true :status :batch-complete})]
      (sut/set-alight-problem-list!
       {:problems [problem]
        :authority {:agent "claude-19" :control-root "/home/joe/code/futon3c"
                    :campaign-root root}
        :autonomous? true})
      (is (= [live] (:conditions @adapter-config))
          "the in-force entries reach the adapter (and so the mint)"))
    (spit (str root "/conditions.edn") "{:not :a :vector}")
    (is (= :registry-error (:kind (first (sut/campaign-conditions root))))
        "an ill-formed registry is recorded as a condition, not dropped")
    (spit (str root "/conditions.edn") "[{:unbalanced")
    (is (= :registry-error (:kind (first (sut/campaign-conditions root))))
        "an unreadable registry is recorded as a condition, not an exception")))

(deftest jit-queue-wires-concrete-adapter-and-countdown-supervision
  (let [adapter-config (atom nil)
        tick-options (atom nil)
        supervised-calls (atom 0)]
    (with-redefs [queued-frame-adapter/live-effects
                  (fn [config]
                    (reset! adapter-config config)
                    {:mint-frame-fn identity})
                  problem-queue/tick!
                  (fn [options]
                    (reset! tick-options options)
                    {:ok true :status :frame-prepared})
                  sut/set-alight!
                  (fn [& _]
                    (swap! supervised-calls inc)
                    {:ok true :status :frame-complete})
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
               {:state-directory "/tmp"
                :ledger-path "/tmp/ledger"
                :preparation-path "/tmp/preparation"}))))
      (is (zero? @supervised-calls)
          "an already terminal ledger bypasses obsolete launch authority")
      (is (fn? (:mint-frame-fn @tick-options)))
      (is (nil? (:jit/config @tick-options))))))

(deftest jit-queue-frame-tick-supplies-one-off-boot-to-supervision
  (let [adapter-config (atom nil)
        supervised-options (atom nil)
        state-directory
        (str (java.nio.file.Files/createTempDirectory
              "jit-f51-no-terminal-"
              (make-array java.nio.file.attribute.FileAttribute 0)))
        manifest {:manifest/id "jit-f51-manifest" :manifest/scope :one-off}
        contract {:contract/id :jit-contract}
        booted {:ok true :status :already-registered}]
    (with-redefs [queued-frame-adapter/live-effects
                  (fn [config]
                    (reset! adapter-config config)
                    {:mint-frame-fn identity})
                  problem-queue/tick! (constantly {:ok true :status :frame-prepared})
                  ledger/read-ledger (constantly {:ok true :events []})
                  runtime/read-state (constantly {:preparation/version 2})
                  queued-frame-adapter/terminal-from-ledger
                  (constantly {:ok false :error/code :frame-not-terminal})
                  sut/bootstrap-one-off!
                  (fn [actual-manifest actual-contract]
                    (is (= manifest actual-manifest))
                    (is (= contract actual-contract))
                    booted)
                  sut/set-alight!
                  (fn [_ options]
                    (reset! supervised-options options)
                    {:ok true :status :awaiting})]
      (sut/set-alight-problem-queue!
       {:problems [{:problem/id "p1" :repository "/repo" :revision "r"
                    :path "p.lean" :blob "b" :classification :non-excluded}]
        :authority {:agent "codex-12"}}
       {:jit/config {:campaign-root "/campaigns"}})
      ((:frame-tick-fn @adapter-config)
       {:frame/id "f51" :problem/id "p1"}
       {:state-directory state-directory
        :ledger-path "/tmp/jit-f51-ledger"
        :preparation-path "/tmp/jit-f51-preparation"
        :manifest manifest :contract contract})
      (is (= booted (:booted @supervised-options))))))

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
                                    "/home/joe/code/futon3c"
                                    :apparatus-root
                                    "/home/joe/code/futon3c-apm-control"
                                    :campaign-root "/durable/f25"}}))))
      (let [config (get-in @captured [:effects :jit/config])]
        (is (every? fn? (map config [:manifest-fn :open-frame-fn :ledger-fn
                                     :retirement-audit-fn])))
        (is (some #{:promote-solver} (get-in config [:contract :phase-order]))
            "the JIT contract is loaded under its v2 path before registration")
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

(deftest retirement-job-ownership-ignores-diagnostic-history
  (let [frame {:frame/id "f27"}
        diagnostic {:state "running" :agent-id "codex-10"
                    :events [{:type "tool_use"
                              :text "inspected /workspaces/f27-p-student"}]}
        role-job {:state "parked" :agent-id "f27-student"}]
    (is (false? (#'sut/active-frame-job? frame diagnostic)))
    (is (#'sut/active-frame-job? frame role-job))
    (is (false? (#'sut/active-frame-job?
                 frame (assoc role-job :state "done"))))))

(deftest memory-cascade-arm-file-is-read-only-when-launch-has-no-arm
  (let [dir (java.nio.file.Files/createTempDirectory
             "memory-cascade-arm-" (make-array java.nio.file.attribute.FileAttribute 0))
        root (str dir)
        arm {:enabled? true :routes [:sibling] :cap 100}]
    (is (nil? (sut/memory-cascade-arm nil root)) "no file, no explicit => off")
    (spit (str root "/memory-cascade-arm.edn") (pr-str arm))
    (is (= arm (sut/memory-cascade-arm nil root)) "file read when launch has no arm")
    (is (= {:enabled? false} (sut/memory-cascade-arm {:enabled? false} root))
        "an explicit launch value wins over the file")
    (spit (str root "/memory-cascade-arm.edn") "[:not :a :map]")
    (is (nil? (sut/memory-cascade-arm nil root)) "a non-map file is off")
    (spit (str root "/memory-cascade-arm.edn") "{:unbalanced")
    (is (nil? (sut/memory-cascade-arm nil root)) "an unreadable file is off, not an exception")))

;; 0555a316 kept the JIT registration path off the countdown manifest, but
;; advance! re-derived the boot by calling bootstrap! itself, so the JIT frame
;; effect still validated the F19-era countdown manifest transitively and threw
;; "Countdown manifest failed executable validation" on every tick after f50
;; closed. A caller that already booted passes its own result in.
(deftest advance-uses-a-supplied-boot-instead-of-rereading-the-manifest
  (let [bootstrap-calls (atom 0)]
    (with-redefs [sut/bootstrap! (fn [] (swap! bootstrap-calls inc) {:ok true})
                  sut/inspect! (constantly {:ok true :stepper/status :blocked})]
      ;; supplied boot: bootstrap! must not be consulted
      (sut/advance! :open-frame nil {:ok true})
      (is (zero? @bootstrap-calls))
      ;; no boot supplied: existing callers keep their behaviour
      (sut/advance! :open-frame)
      (is (= 1 @bootstrap-calls)))))
