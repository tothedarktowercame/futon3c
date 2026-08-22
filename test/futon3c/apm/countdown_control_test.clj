(ns futon3c.apm.countdown-control-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.countdown-control :as sut]
            [futon3c.apm.live-promotion :as live-promotion]))

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
                                        {:path "proctor.md" :blob "blob"}}}}
                :unit {:frame/id "f22" :problem/id "p22"}
                :preparation {:seats {:proctor {:agent-id "f22-proctor"}}
                              :seat-policy {:turn-timeout-ms 7200000}}}]
    (with-redefs [sut/live-learning-phase-inputs (constantly inputs)
                  live-promotion/run-live!
                  (fn [opts] (reset! captured opts)
                    {:ok true :status :awaiting-terminal :job-id "scribe-job"})]
      (is (= "scribe-job" (:job-id (sut/drive-live-learning-phase! action))))
      (is (= "f22-proctor" (get-in @captured [:reviewer-request :agent-id])))
      (is (= "blob" (get-in @captured [:reviewer-request :role-card-blob])))
      (is (= 7200000
             (get-in @captured [:reviewer-request :turn-timeout-ms])))
      (is (fn? (:publish-fn @captured))))))

(deftest replacement-registration-starts-at-f19-with-complete-cycle
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
         {:regulator-id sut/machine-regulator-id
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
    (is (true? (authorized? sut/machine-regulator-id capability)))))

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
                             :proctor :codex :scribe :zai}))}]
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
