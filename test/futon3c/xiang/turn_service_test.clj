(ns futon3c.xiang.turn-service-test
  "The 象 pipeline with every effect faked: bells, job status, the
   withdrawal routes and the clock. The scheduler is a queue the test
   drains by hand, so the reap cadence is asserted, not waited for."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.turn-record :as tr]
            [futon3c.xiang.turn-store :as ts]
            [futon3c.xiang.turn-service :as svc]))

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "xiang-svc-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- harness
  "A service plus the atoms its fakes write to."
  [& [overrides]]
  (let [bells (atom []) scheduled (atom []) jobs (atom {}) posted (atom [])
        clock (atom 1000000)
        s (svc/service (merge {:store (temp-store)
                               :now-ms (fn [] @clock)
                               :schedule! (fn [delay thunk] (swap! scheduled conj [delay thunk]))
                               :bell! (fn [payload]
                                        (swap! bells conj payload)
                                        {:ok true :job-id (str "job-" (count @bells))})
                               :job-status (fn [job-id] (get @jobs job-id))
                               :post-withdrawal (fn [p] (swap! posted conj [:withdrawal p])
                                                  {:status 200 :json {:record {:id "act:w1"}}})
                               :post-negation (fn [p] (swap! posted conj [:negation p])
                                                {:status 200 :json {:entry {:evidence/id "neg-1"
                                                                            :evidence/body {:resolution "target-unresolved"}}}})
                               :deliver-notice (fn [p] (swap! posted conj [:notice p]) {:status 200})}
                              overrides))]
    {:svc s :bells bells :scheduled scheduled :jobs jobs :posted posted :clock clock}))

(defn- run-next!
  "Run the oldest scheduled thunk; returns [delay result]."
  [{:keys [scheduled]}]
  (let [[delay thunk] (first @scheduled)]
    (swap! scheduled subvec 1)
    [delay (thunk)]))

(defn- delays [h] (mapv first @(:scheduled h)))

(defn- turn! [h & [opts]]
  (svc/record-turn! (:svc h) (merge {:text "Please continue. I withdraw that pattern."
                                     :agent-id "claude-17" :session-id "sess-1" :turn-id "claude-17-turn-3"
                                     :evidence-id "emacs-abc"}
                                    opts)))

(deftest a-turn-is-recorded-at-send-and-dispatched-at-reply-end
  (let [h (harness)
        {:keys [id dispatch record]} (turn! h)]
    (is (= :pending dispatch))
    (is (= "requested" (:analysis_status record)))
    (is (empty? @(:bells h)))
    (let [result (svc/attach-happened! (:svc h) id {:reply "line1\nline2\nline3\nline4\nline5\nline6"
                                                    :commits [{:repo "futon3c" :sha "abc123def456" :subject "fix"
                                                               :numstat "3\t1\tfoo.clj\n-\t-\tbin.png\n"}]})]
      (is (:dispatched result))
      (is (= "job-1" (:job-id result)))
      (is (= "象-1" (:agent result)) "the least-loaded pool seat")
      (let [bell (first @(:bells h))
            stored (ts/read-record (:config (:svc h) :store) id)
            stored (ts/read-record (get-in (:svc h) [:config :store]) id)]
        (is (= "象-1" (:agent-id bell)))
        (is (= "turn-capture" (:caller bell)))
        (is (= "work" (:mode bell)))
        (is (str/starts-with? (:prompt bell) (str "Requisition: M-futon-seams — interpret operator turn " id)))
        (is (str/includes? (:prompt bell) (ts/record-path (get-in (:svc h) [:config :store]) id)))
        (is (= "job-1" (get-in stored [:analysis_dispatch :job_id])))
        (is (str/includes? (:happened_summary stored) "line5"))
        (is (not (str/includes? (:happened_summary stored) "line6")))
        (is (str/includes? (:happened_summary stored) "- futon3c abc123de fix (+3 -1 over 2 files)")))
      (is (= [180] (delays h)) "the first reap waits reap-after"))))

(deftest a-turn-addressed-to-the-seat-is-recorded-but-never-dispatched
  (let [h (harness)
        {:keys [id dispatch]} (turn! h {:agent-id "象"})]
    (is (= :skipped dispatch))
    (is (= {:dispatched false :reason :addressed-to-seat} (svc/attach-happened! (:svc h) id "summary")))
    (is (empty? @(:bells h)))))

(deftest an-external-turn-dispatches-at-once-and-a-failed-send-stays-requested
  (let [h (harness {:bell! (fn [_] {:ok false :status 404 :error "agent not registered"})})
        {:keys [id dispatch]} (turn! h {:dispatch :now})]
    (is (false? (:dispatched dispatch)))
    (is (re-find #"http 404" (:reason dispatch)))
    (is (= :failing (:state (svc/health (:svc h)))))
    (is (= "requested" (:analysis_status (ts/read-record (get-in (:svc h) [:config :store]) id))))
    (is (nil? (get-in (ts/read-record (get-in (:svc h) [:config :store]) id) [:analysis_dispatch :job_id])))
    (is (empty? (delays h)))))

(deftest reaping-a-running-job-follows-the-emacs-cadence
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :now})]
    (swap! (:jobs h) assoc "job-1" {:state "running"})
    (is (= [180] (delays h)))
    (dotimes [_ 3] (is (= 180 (first (run-next! h)))))
    (dotimes [_ 5] (is (= 600 (first (run-next! h)))))
    (is (= [600] (delays h)))
    (is (= [600 {:outcome :running :state "running" :tries-left 0}] (run-next! h)))
    (is (empty? (delays h)) "the schedule is bounded")
    (is (= "requested" (:analysis_status (ts/read-record (get-in (:svc h) [:config :store]) id))))))

(deftest a-refusal-is-written-with-its-reason
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :now})]
    (swap! (:jobs h) assoc "job-1" {:state "done" :execution {:executed false}
                                    :events [{:type "refusal" :text "no requisition line"}]})
    (is (= [180 {:outcome :refused :state "done" :reason "refusal: no requisition line"}]
           (run-next! h)))
    (let [rec (ts/read-record (get-in (:svc h) [:config :store]) id)]
      (is (= "refused" (:analysis_status rec)))
      (is (= {:state "done" :reason "refusal: no requisition line"} (get-in rec [:analysis_dispatch :outcome]))))
    (is (= :failing (:state (svc/health (:svc h)))))
    (testing "a job the ledger no longer knows is unreachable, not refused"
      (let [{:keys [id]} (turn! h {:dispatch :now})]
        (is (= :unreachable (:outcome (svc/reap! (:svc h) id {}))))
        (is (= "requested" (:analysis_status (ts/read-record (get-in (:svc h) [:config :store]) id))))))))

(deftest a-seat-out-of-usage-is-benched-and-the-turn-goes-elsewhere
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :now})]
    (swap! (:jobs h) assoc "job-1" {:state "failed" :events [{:type "error" :error "usage limit reached"}]})
    (is (= :benched (:outcome (second (run-next! h)))))
    (is (every? #(svc/benched? (:svc h) %) ["象-1" "象-2" "象-3" "象-4"]) "a pool seat benches the pool")
    (is (= "象-sonnet" (:agent-id (second @(:bells h)))))
    (let [rec (ts/read-record (get-in (:svc h) [:config :store]) id)]
      (is (= "requested" (:analysis_status rec)))
      (is (= "job-2" (get-in rec [:analysis_dispatch :job_id])))
      (is (= [{:job_id "job-1"}] (get-in rec [:analysis_dispatch :attempts])) "the old attempt is kept"))
    (is (= "象-sonnet" (svc/analysis-seat (:svc h))))
    (swap! (:clock h) + (* 61 60000))
    (is (= "象-1" (svc/analysis-seat (:svc h))) "the bench expires")))

(deftest a-busy-store-retries-on-the-bounded-schedule
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :now})
        busy! (fn [job] (swap! (:jobs h) assoc job {:state "failed" :events [{:type "error" :error "Turn not started: futon1b busy"}]}))]
    (busy! "job-1")
    (is (= [180 {:outcome :store-busy :retry-in-s 60}] (run-next! h)))
    (is (= [60] (delays h)))
    (run-next! h)
    (is (= 2 (count @(:bells h))) "redispatched to the same seat")
    (is (= "象-1" (:agent-id (second @(:bells h)))))
    (is (= [180] (delays h)) "and a reap is scheduled for the new job")
    (busy! "job-2")
    (is (= [180 {:outcome :store-busy :retry-in-s 180}] (run-next! h)))
    (run-next! h)
    (busy! "job-3")
    (is (= [180 {:outcome :store-busy :retry-in-s 600}] (run-next! h)))
    (run-next! h)
    (busy! "job-4")
    (is (= :failed (:outcome (second (run-next! h)))) "the schedule is exhausted")
    (let [rec (ts/read-record (get-in (:svc h) [:config :store]) id)]
      (is (= "failed" (:analysis_status rec)))
      (is (= 3 (count (get-in rec [:analysis_dispatch :attempts]))))
      (is (= "job-4" (get-in rec [:analysis_dispatch :job_id]))))))

(defn- analysis-for [record]
  (let [[s1 s2] (:sentences record)]
    {:labeller "象-1"
     :sentences [{:id "s1" :fragments [{:start (:start s1) :end (:end s1) :text (:text s1)
                                        :intent "continue" :target "the work" :rationale "r"
                                        :relations ["action"]
                                        :display_cues [{:start 0 :end 15 :text "Please continue"}]}]}
                 {:id "s2" :fragments [{:start (:start s2) :end (:end s2) :text (:text s2)
                                        :intent "withdraw" :target nil :rationale "r"
                                        :relations ["action"] :display_cues [] :no_surface_cue "implicit"}]}]}))

(deftest publishing-an-analysis-processes-its-withdrawals
  (let [h (harness)
        {:keys [id record]} (turn! h {:dispatch :now})
        store (get-in (:svc h) [:config :store])
        out (svc/publish-analysis! (:svc h) id (analysis-for record) {})]
    (is (= "analyzed" (get-in out [:analysis :status])))
    (is (ts/analysis-published? store id))
    (let [rec (ts/read-record store id)
          effects (:withdrawal_effects rec)
          negations (:negation_interpretations rec)]
      (is (= "analyzed" (:analysis_status rec)))
      (is (= 1 (count effects)))
      (is (= {:fragment_id "s2:0" :status 422 :reason "target-unresolved"
              :idempotency_key (str id ":s2:0")}
             (dissoc (first effects) :header_notice_published_at)))
      (is (string? (:header_notice_published_at (first effects))) "the notice was published once")
      (is (= [{:fragment_id "s2:0" :status 200 :evidence_id "neg-1" :resolution "target-unresolved" :reason nil}]
             negations))
      (is (nil? (:withdrawal_processing_error rec))))
    (testing "the posted payloads are the Emacs ones"
      (let [by-kind (group-by first @(:posted h))]
        (is (empty? (:withdrawal by-kind)) "a null target posts no withdrawal")
        (is (= {:caller "xiang" :operator-evidence-id "emacs-abc" :fragment-id "s2:0"
                :fragment-text "I withdraw that pattern." :analysis-version 3}
               (second (first (:negation by-kind)))))
        (is (= {:caller "xiang" :agent "claude-17" :session "sess-1"
                :notice-id (str id ":s2:0") :kind "unresolved"}
               (second (first (:notice by-kind)))))))
    (testing "the view carries the notice a REPL would have shown"
      (is (= [{:kind "unresolved" :text "withdraw inferred: unresolved (no target)" :fragment_id "s2:0"}]
             (:notices (svc/turn-view (:svc h) id)))))
    (testing "a second publication is refused and nothing is re-posted"
      (let [n (count @(:posted h))]
        (is (= :analysis-exists
               (try (svc/publish-analysis! (:svc h) id (analysis-for record) {}) nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (= n (count @(:posted h))))))
    (testing "a reap after publication reports analyzed without re-posting"
      (let [n (count @(:posted h))]
        (is (= :analyzed (:outcome (svc/reap! (:svc h) id {}))))
        (is (= n (count @(:posted h))))))
    (is (= :ok (:state (svc/health (:svc h)))))))

(deftest a-withdrawal-with-a-target-is-posted-and-its-effect-noticed
  (let [h (harness)
        {:keys [id record]} (turn! h {:dispatch :now})
        analysis (assoc-in (analysis-for record) [:sentences 1 :fragments 0 :target] "act:old")]
    (svc/publish-analysis! (:svc h) id analysis {})
    (let [[kind payload] (first (filter #(= :withdrawal (first %)) @(:posted h)))
          effect (first (:withdrawal_effects (ts/read-record (get-in (:svc h) [:config :store]) id)))]
      (is (= :withdrawal kind))
      (is (= {:caller "xiang" :agent "claude-17" :session "sess-1"
              :interpretation-id "claude-17-turn-3" :interpretation-version 3
              :idempotency-key (str id ":s2:0") :target "act:old"}
             payload))
      (is (= 200 (:status effect)))
      (is (= "act:w1" (:effect_id effect)))
      (is (= [{:kind "effect" :effect_id "act:w1" :text "withdraw inferred: effect act:w1 (undo to reverse)"
               :fragment_id "s2:0"}]
             (:notices (svc/turn-view (:svc h) id))))
      (is (= {:caller "xiang" :agent "claude-17" :session "sess-1" :notice-id (str id ":s2:0")
              :kind "effect" :effect-id "act:w1"}
             (second (first (filter #(= :notice (first %)) @(:posted h)))))))))

(deftest no-grant-is-said-once-and-a-failed-notice-is-retried-then-given-up
  (let [h (harness {:post-withdrawal (fn [_] {:status 403 :json {:reason :no-grant}})
                    :deliver-notice (fn [_] {:status 500 :error "store down"})})
        {:keys [id record]} (turn! h {:dispatch :now})
        analysis (assoc-in (analysis-for record) [:sentences 1 :fragments 0 :target] "seat-active-card")]
    (svc/publish-analysis! (:svc h) id analysis {})
    (is (true? (:notified-no-grant @(:state (:svc h)))) "said once, in state")
    (let [effect #(first (:withdrawal_effects (ts/read-record (get-in (:svc h) [:config :store]) id)))]
      (is (= 403 (:status (effect))))
      (is (= "no-grant" (:reason (effect))))
      (is (= 1 (:header_notice_attempts (effect))))
      (dotimes [_ 4] (svc/process-withdrawals! (:svc h) id))
      (is (= 5 (:header_notice_attempts (effect))))
      (is (= "http-500:store down" (:header_notice_give_up_reason (effect))))
      (svc/process-withdrawals! (:svc h) id)
      (is (= 5 (:header_notice_attempts (effect))) "given up means no more attempts"))))

(deftest an-invalid-analysis-is-refused-before-anything-is-written
  (let [h (harness)
        {:keys [id record]} (turn! h {:dispatch :now})
        bad (assoc-in (analysis-for record) [:sentences 0 :fragments 0 :intent] "Not A Label")]
    (is (= :invalid-analysis
           (try (svc/publish-analysis! (:svc h) id bad {}) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    (is (not (ts/analysis-published? (get-in (:svc h) [:config :store]) id)))
    (is (empty? @(:posted h)))))

(deftest the-seat-conversation-is-reset-every-n-dispatches
  (let [resets (atom [])
        h (harness {:reset-every 2 :reset-seat! (fn [a] (swap! resets conj a))})]
    (dotimes [_ 3] (turn! h {:dispatch :now}))
    (is (= ["象-3"] @resets) "the third dispatch resets the seat it goes to (the least-loaded one)")
    (is (= 1 (:dispatch-count @(:state (:svc h)))))))

(deftest happened-summary-shape
  (is (= "What the agent did (machine-added context for reading the turn):\nReply begins:\nreply\nCommits during the turn (any repo; not necessarily the agent's own):\n(none)"
         (svc/happened-summary "reply" [])))
  (let [many (for [i (range 1 24)] {:repo "r" :sha (format "%040d" i) :subject (str "c" i)})
        s (svc/happened-summary "x" many)]
    (is (str/includes? s "…and 3 more"))
    (is (str/includes? s "c20"))
    (is (not (str/includes? s "c21"))))
  (is (= [5 1 3] (svc/summarize-numstat "3\t1\tfoo.el\n-\t-\tbin.png\n2\t0\tbar.py\n"))))
