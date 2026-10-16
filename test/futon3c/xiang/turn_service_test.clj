(ns futon3c.xiang.turn-service-test
  "The 象 pipeline with every effect faked: bells, job status, the
   withdrawal routes and the clock. The scheduler is a queue the test
   drains by hand, so the reap cadence is asserted, not waited for."
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as registry]
            [futon3c.agency.session-end-test-fixture :as session-end-fixture]
            [futon3c.xiang.turn-record :as tr]
            [futon3c.xiang.turn-store :as ts]
            [futon3c.xiang.turn-acts :as turn-acts]
            [futon3c.xiang.turn-service :as svc]))

(use-fixtures :each session-end-fixture/with-isolated-session-end-outbox)
(use-fixtures :each
  (fn [f]
    ;; transport.http installs its production listener at namespace load. These
    ;; service-unit tests own the registry and add only the listener they assert.
    (svc/remove-turn-listener! :futon3c.transport.http/frames-cache)
    (svc/remove-turn-listener! ::test)
    (svc/remove-turn-listener! ::bad)
    (f)))

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
                               ;; Run the evidence append inline, so the
                               ;; schedule assertions see only the pipeline.
                               :evidence-async! (fn [thunk] (thunk))
                               :draft-async! (fn [thunk] (thunk))
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

(declare analysis-for)

(deftest turn-listeners-are-off-path-and-carry-lifecycle-events
  (let [events (atom []) h (harness)]
    (svc/add-turn-listener! ::test #(swap! events conj %))
    (try
      (let [result (turn! h {:dispatch :none})]
        (is (= :declared (:dispatch result)))
        (is (empty? @events) "record notification is scheduled, not on the caller path")
        (run-next! h)
        (is (= [{:event :recorded :id (:id result) :session-id "sess-1"
                 :agent-id "claude-17" :how nil}]
               @events)))
      (finally (svc/remove-turn-listener! ::test)))))

(deftest settling-notifies-reading-then-settled
  (let [events (atom []) h (harness)
        {:keys [id record]} (turn! h {:dispatch :none})
        store (get-in (:svc h) [:config :store])]
    ;; Discard the unobserved recorded notification, then make the evidence
    ;; packet complete so settle writes immediately in this synchronous harness.
    (reset! (:scheduled h) [])
    (ts/update-record! store id #(assoc % :happened_summary "done"))
    (svc/add-turn-listener! ::test #(swap! events conj (:event %)))
    (try
      (svc/publish-analysis! (:svc h) id (analysis-for record) {})
      (while (seq @(:scheduled h)) (run-next! h))
      (is (= [:reading :settled] @events))
      (finally (svc/remove-turn-listener! ::test)))))

(deftest throwing-turn-listener-never-fails-recording
  (let [h (harness)]
    (svc/add-turn-listener! ::bad (fn [_] (throw (ex-info "listener broke" {}))))
    (try
      (let [result (turn! h {:dispatch :none})]
        (is (= :declared (:dispatch result)))
        (is (string? (:id result)))
        (run-next! h)
        (is (str/includes? (:detail (svc/health (:svc h))) "listener broke")))
      (finally (svc/remove-turn-listener! ::bad)))))

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
            stored (ts/read-record (get-in (:svc h) [:config :store]) id)]
        (is (= "象-1" (:agent-id bell)))
        (is (= "turn-capture" (:caller bell)))
        (is (= "work" (:mode bell)))
        (is (str/starts-with? (:prompt bell) (str "Requisition: M-futon-seams — interpret operator turn " id)))
        (is (str/includes? (:prompt bell) (ts/record-path (get-in (:svc h) [:config :store]) id)))
        (is (= "job-1" (get-in stored [:analysis_dispatch :job_id])))
        (is (= "1970-01-01T00:16:40Z" (:reply_at stored)))
        (is (str/includes? (:happened_summary stored) "line5"))
        (is (not (str/includes? (:happened_summary stored) "line6")))
        (is (str/includes? (:happened_summary stored) "- futon3c abc123de fix (+3 -1 over 2 files)")))
      (is (= [180] (delays h)) "the first reap waits reap-after"))))

(deftest happened-preserves-an-explicit-reply-time
  (let [h (harness)
        {:keys [id]} (turn! h)
        reply-at "2026-10-11T06:30:45Z"]
    (svc/attach-happened! (:svc h) id {:reply "done" :reply-at reply-at :commits []})
    (let [record (ts/read-record (get-in (:svc h) [:config :store]) id)]
      (is (= reply-at (:reply_at record)))
      (is (instance? java.time.Instant (java.time.Instant/parse (:reply_at record)))))))

(deftest a-turn-addressed-to-the-seat-is-recorded-but-never-dispatched
  (let [h (harness)
        {:keys [id dispatch]} (turn! h {:agent-id "象"})]
    (is (= :skipped dispatch))
    (is (= {:dispatched false :reason :addressed-to-seat} (svc/attach-happened! (:svc h) id "summary")))
    (is (nil? (:reply_at (ts/read-record (get-in (:svc h) [:config :store]) id))))
    (is (empty? @(:bells h)))))

(deftest a-declared-turn-is-recorded-settled-and-never-dispatched
  ;; M-象-2000, Joe 2026-10-02: the bridges' agent replies are recorded with
  ;; dispatch :none — stored with their proforma_marks, never read by 象, and
  ;; no later dispatch or retry may queue them.
  (let [h (harness)
        {:keys [id dispatch record]} (turn! h {:dispatch :none :origin "agent"
                                               :text "㊥ Done.\n\n🈸 Shall I go on?"})]
    (is (= :declared dispatch))
    (is (= "declared" (:analysis_status record)))
    (is (= "agent" (:origin record)))
    (is (seq (:proforma_marks record)) "the author's own marks are stored")
    (is (empty? @(:bells h)))
    (is (= {:dispatched false :reason :declared} (svc/dispatch! (:svc h) id {})))
    (svc/retry! (:svc h) id)
    (is (= "declared" (:analysis_status (ts/read-record (get-in (:svc h) [:config :store]) id)))
        "retry leaves a declared record settled")
    (is (empty? @(:bells h)))))

(deftest records-carry-the-recipient-mission-from-the-injected-roster
  (let [roster (fn [agent-id]
                 (when (= "claude-17" agent-id)
                   {:agent/metadata {:mission-id "  M-warp  "}}))
        h (harness {:agent-roster roster})]
    (testing "operator turn"
      (is (= "M-warp" (:mission_id (:record (turn! h))))))
    (testing "agent-origin bell turn"
      (let [{:keys [record]} (turn! h {:dispatch :none :origin "agent"})]
        (is (= "M-warp" (:mission_id record)))))))

(deftest blank-or-absent-roster-missions-stay-absent
  (doseq [mission-id [::no-agent nil "" "  "]]
    (let [h (harness {:agent-roster (if (= ::no-agent mission-id)
                                      (constantly nil)
                                      (constantly {:agent/metadata
                                                   {:mission-id mission-id}}))})
          record (:record (turn! h))]
      (is (not (contains? record :mission_id)) (pr-str mission-id)))))

(deftest a-not-requested-turn-is-never-dispatched
  ;; 象-off (session-turn-analysis.el, policy 'never') records turns with
  ;; analysis_status "not-requested". dispatch! must not queue them — before
  ;; this guard the seats were belled anyway (misfire, Joe 2026-10-03).
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :later})]
    (ts/update-record! (get-in (:svc h) [:config :store]) id
                       #(assoc % :analysis_status "not-requested"))
    (is (= {:dispatched false :reason :not-requested} (svc/dispatch! (:svc h) id {})))
    (is (empty? @(:bells h)))))

(deftest a-turn-recorded-with-analysis-not-requested-is-never-dispatched
  ;; M-象-2000 step 1: under the `jvm' recorder Emacs sends the policy
  ;; decision with the turn; with policy `never' (象-off) the flag is false,
  ;; the record comes back not-requested, and no dispatch, happened or retry
  ;; may bell a seat.
  (let [h (harness)
        {:keys [id dispatch record path]} (turn! h {:analysis-requested? (constantly false)})]
    (is (= :pending dispatch))
    (is (= "not-requested" (:analysis_status record)))
    (is (string? path))
    (is (empty? @(:bells h)))
    (is (= {:dispatched false :reason :not-requested}
           (svc/attach-happened! (:svc h) id {:reply "done" :commits []})))
    (is (= {:dispatched false :reason :not-requested}
           (svc/dispatch! (:svc h) id {})))
    (is (empty? @(:bells h)))
    (is (= "not-requested"
           (:analysis_status (ts/read-record (get-in (:svc h) [:config :store]) id)))
        "the stored record keeps the policy's answer")))

(deftest a-soon-turn-is-answered-at-once-and-dispatched-off-the-path
  ;; The Emacs jvm recorder sends "soon": the 小象 draft is tried inline
  ;; (bounded, M-象-2000), then dispatch! runs on the scheduler at delay 0.
  (let [drafted (atom 0)
        h (harness {:draft (fn [_] (swap! drafted inc) nil)})
        {:keys [id dispatch draft]} (turn! h {:dispatch :soon})]
    (is (= :scheduled dispatch))
    (is (false? draft) "the draft effect returned nothing")
    (is (= 1 @drafted) "the draft ran inline (it is usually ~0.1 s)")
    (is (empty? @(:bells h)) "but the seat was not belled yet")
    (is (= [0] (delays h)))
    (run-next! h)
    (is (= 1 (count @(:bells h))) "the reading starts at send")
    (is (= "job-1" (get-in (ts/read-record (get-in (:svc h) [:config :store]) id)
                           [:analysis_dispatch :job_id])))))

(deftest happened-after-dispatch-does-not-dispatch-again
  ;; Planted: count the bells. A turn already on its way to a seat gets its
  ;; classical summary attached, never a second dispatch.
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :soon})]
    (run-next! h)
    (is (= 1 (count @(:bells h))))
    (is (= {:dispatched false :reason :already-dispatched}
           (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})))
    (is (= 1 (count @(:bells h))))
    (is (some? (:happened_summary (ts/read-record (get-in (:svc h) [:config :store]) id)))
        "the summary still lands on the record")))

(deftest a-not-requested-turn-with-soon-never-bells
  ;; 象-off: Emacs keeps sending the turn, with analysis-requested false;
  ;; even "soon" must not queue it.
  (let [h (harness)
        {:keys [id dispatch]} (turn! h {:dispatch :soon :analysis-requested? (constantly false)})]
    (is (= :scheduled dispatch))
    (run-next! h)
    (is (empty? @(:bells h)))
    (is (= "not-requested"
           (:analysis_status (ts/read-record (get-in (:svc h) [:config :store]) id))))
    (is (= {:dispatched false :reason :not-requested}
           (svc/attach-happened! (:svc h) id "did things")))))

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

(deftest reading-a-turn-settles-it-behind-a-dead-job
  ;; 2026-10-16: a restart dropped the scheduled reaps and left the jobs
  ;; "delivering"; the records stayed requested while Emacs polled them.
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :now})
        store (get-in (:svc h) [:config :store])]
    (swap! (:jobs h) assoc "job-1" {:state "running"})
    (is (= "requested" (get-in (svc/turn-view (:svc h) id) [:record :analysis_status])))
    (is (= [180] (delays h)) "a read of a running job schedules nothing more")
    (swap! (:jobs h) assoc "job-1" {:state "delivering" :terminal-code "worker-lost-on-restart"
                                    :events [{:type "running"} {:type "failed" :code "worker-lost-on-restart"}]})
    (is (= "failed" (get-in (svc/turn-view (:svc h) id) [:record :analysis_status])))
    (is (= "failed" (:analysis_status (ts/read-record store id))))))

(deftest reading-a-turn-behind-a-quota-failure-benches-and-fails-over
  ;; 2026-10-16: the reap on read passed no :agent, so a Kimi 403 settled the
  ;; turn as failed and the pool was never benched; every turn from 04:52Z to
  ;; 05:49Z went to 象-1 and failed the same way while 象-sonnet sat idle.
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :now})
        store (get-in (:svc h) [:config :store])]
    (swap! (:jobs h) assoc "job-1"
           {:state "failed" :agent-id "象-1"
            :events [{:type "prompt" :text "Requisition: interpret operator turn"}
                     {:type "failed" :code "invoke-error"
                      :message "{\"message\":\"HTTP 403\",\"body\":{\"error\":{\"message\":\"You've reached your monthly usage limit for this billing cycle.\"}}}"}
                     {:type "delivery-recorded" :status "delivery-failed" :note "caller-not-a-registered-seat"}]})
    (svc/turn-view (:svc h) id)
    (is (every? #(svc/benched? (:svc h) %) ["象-1" "象-2" "象-3" "象-4"]) "the read benches the pool")
    (is (= "象-sonnet" (:agent-id (second @(:bells h)))) "and the alternate takes the turn")
    (let [rec (ts/read-record store id)]
      (is (= "requested" (:analysis_status rec)))
      (is (= "job-2" (get-in rec [:analysis_dispatch :job_id]))))))

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

(deftest an-agent-turn-gets-the-agent-brief-and-no-withdrawals
  (let [h (harness)
        {:keys [id record]} (svc/record-turn! (:svc h) {:text "㊥ Done.\n\n🈡 I withdraw that pattern."
                                                        :origin "agent" :agent-id "claude-17"
                                                        :session-id "sess-1" :turn-id "t-reply" :dispatch :now})]
    (is (= ["gist" "withdraw"] (map :intent (:proforma_marks record))))
    (is (str/starts-with? (:prompt (first @(:bells h))) (str "Requisition: M-futon-seams — interpret agent turn " id)))
    (let [[s1 s2] (:sentences record)
          analysis {:labeller "象-1"
                    :sentences [{:id "s1" :fragments [{:start (:start s1) :end (:end s1) :text (:text s1) :intent "gist"
                                                       :target "t" :rationale "r" :relations ["action"]
                                                       :display_cues [] :no_surface_cue "x"}]}
                                {:id "s2" :fragments [{:start (:start s2) :end (:end s2) :text (:text s2) :intent "withdraw"
                                                       :target nil :rationale "r" :relations ["action"]
                                                       :display_cues [] :no_surface_cue "x"}]}]}]
      (svc/publish-analysis! (:svc h) id analysis {})
      (is (empty? @(:posted h)) "withdrawals are inferred for operator turns only")
      (is (nil? (:withdrawal_effects (ts/read-record (get-in (:svc h) [:config :store]) id)))))))

;; ---------------------------------------------------------------------------
;; 小象 drafts in the pipeline

(defn- fake-draft [text]
  ;; 小象 over the record's source text: one sure fragment per sentence, its
  ;; intent the sentence's first lexical cue, else approve. (A real 小象 that
  ;; mislabels an act as approve would let the skip policy swallow the act;
  ;; that is why the policy ships off and the agreement numbers come first.)
  (mapv (fn [s] {:start (:start s) :end (:end s) :text (:text s)
                 :intent (or (:label (first (:cues s))) "approve")
                 :guesses ["approve" "report"] :precision 0.8})
        (:sentences (tr/structure-turn text))))

(deftest a-draft-is-taken-at-record-time-and-rides-the-brief
  (let [h (harness {:draft fake-draft})
        {:keys [id draft record]} (turn! h {:text "Looks good." :dispatch :now})
        store (get-in (:svc h) [:config :store])]
    (is (true? draft) "draft readiness is returned")
    (is (= "drafted" (:draft_status record)))
    (is (= "小象" (:labeller (ts/read-draft store id))))
    (is (str/includes? (:prompt (first @(:bells h))) "A CLASSICAL DRAFT EXISTS"))
    (is (= "requested" (:analysis_status (ts/read-record store id))) "without the switch, 象 still reads it")
    (testing "a bad draft is rejected and nothing is stored"
      (let [h (harness {:draft (fn [_] [{:start 0 :end 99 :text "x"}])})
            {:keys [id draft]} (turn! h {:text "Hello." :dispatch :now})]
        (is (false? draft))
        (is (nil? (ts/read-draft (get-in (:svc h) [:config :store]) id)))
        (is (re-find #"draft rejected" (:detail (svc/health (:svc h)))))))))

(deftest a-soon-turn-returns-a-fast-draft-inline
  ;; M-象-2000: the POST answers with 小象's draft when it finishes inside
  ;; the bound, so the stepper can show the new frame at RET.
  (let [h (harness {:draft fake-draft})
        {:keys [id dispatch draft]} (turn! h {:text "Looks good." :dispatch :soon})
        store (get-in (:svc h) [:config :store])]
    (is (= :scheduled dispatch))
    (is (true? draft) "the response reports that the draft is ready")
    (is (= "小象" (:labeller (ts/read-draft store id))) "the draft is stored before return")
    (is (empty? @(:bells h)) "dispatch still happens off the request path")))

(deftest a-slow-draft-falls-back-to-the-async-path
  ;; The draft that took 11 s live must not hold the POST: the inline try is
  ;; bounded, the response carries no draft, and the off-path draft still
  ;; lands (for :soon, dispatch! drafts the record itself).
  (let [slow (fn [text] (Thread/sleep 3000) (fake-draft text))
        h (harness {:draft slow})
        started (System/currentTimeMillis)
        {:keys [id dispatch draft]} (turn! h {:text "Looks good." :dispatch :soon})
        elapsed (- (System/currentTimeMillis) started)
        store (get-in (:svc h) [:config :store])]
    (is (= :scheduled dispatch))
    (is (false? draft) "the response reports that the draft is not ready")
    (is (< elapsed 2800)
        (str "the POST is answered inside the inline bound, took " elapsed "ms"))
    (is (nil? (ts/read-draft store id)))
    (Thread/sleep 1200)
    (is (= "小象" (:labeller (ts/read-draft store id)))
        "the timed-out future continued and wrote the draft")))

(deftest the-skip-policy-settles-routine-turns-without-a-reading
  (let [h (harness {:draft fake-draft :skip-routine? true})
        store (get-in (:svc h) [:config :store])
        routine (turn! h {:text "Looks good." :dispatch :now})
        act (turn! h {:text "Looks good. Please withdraw that pattern." :dispatch :now})]
    (is (= {:dispatched false :reason :drafted :agent nil} (:dispatch routine)))
    (is (= "drafted" (:analysis_status (ts/read-record store (:id routine)))))
    (is (:dispatched (:dispatch act)) "a withdraw cue makes the draft act-bearing, so 象 reads it")
    (is (= 1 (count @(:bells h))))
    (is (= {:outcome :analyzed :status "drafted"} (select-keys (svc/reap! (:svc h) (:id routine) {}) [:outcome :status])))))

(deftest the-agreement-aggregate-sums-the-store
  (let [h (harness {:draft fake-draft :skip-routine? true})
        store (get-in (:svc h) [:config :store])
        _ (turn! h {:text "Looks good." :dispatch :now})
        {:keys [id record]} (turn! h {:text "Fine. Please continue." :dispatch :now})
        [s1 s2] (:sentences record)]
    (svc/publish-analysis! (:svc h) id
                           {:labeller "象-1"
                            :sentences [{:id "s1" :fragments [{:start (:start s1) :end (:end s1) :text (:text s1) :intent "approve" :target "t" :rationale "r" :relations ["action"] :display_cues [] :no_surface_cue "x"}]}
                                        ;; 象 reads "Please continue." as ask-action where 小象 drafted continue.
                                        {:id "s2" :fragments [{:start (:start s2) :end (:end s2) :text (:text s2) :intent "ask-action" :target "t" :rationale "r" :relations ["action"] :display_cues [] :no_surface_cue "x"}]}]}
                           {})
    (let [a (svc/draft-agreement (:svc h) :session-id "sess-1")]
      (is (= 2 (:records a)))
      (is (= 1 (:drafted a)))
      (is (= 1 (:analysed a) (:with-draft a)))
      (is (= {:agreed 1 :relabelled 1} (select-keys (:totals a) [:agreed :relabelled]))))
    (is (= ["xiaoxiang" "xiang-relabelled"]
           (map :basis (mapcat :fragments (:sentences (ts/read-analysis store id))))))
    (is (= "小象" (:labeller (:draft (svc/turn-view (:svc h) id)))))))

(deftest pattern-candidates-are-found-once-per-dispatch-and-ride-the-brief
  (let [asked (atom [])
        h (harness {:draft fake-draft
                    :pattern-candidates (fn [queries] (swap! asked conj queries)
                                          (into {} (map (fn [q] [q [{:id "social/x" :score 1.0 :title "X"}]]) queries)))})
        store (get-in (:svc h) [:config :store])
        {:keys [id]} (turn! h {:text "Yes. Please continue with the port." :dispatch :now})]
    (is (= [["Please continue with the port."]] @asked) "the draft's fragments are the queries; a one-word fragment is skipped")
    (is (= [{:id "social/x" :score 1.0 :title "X"}]
           (get (ts/read-pattern-candidates store id) (keyword "Please continue with the port."))))
    (is (str/includes? (:prompt (first @(:bells h))) "PATTERN CANDIDATES WERE PRECOMPUTED"))
    (is (str/includes? (:prompt (first @(:bells h))) "social/x (1.0) X"))
    (is (some? (:pattern_candidates (svc/turn-view (:svc h) id)))))
  (testing "without a draft the sentences are the queries; a failing finder is logged and the brief still goes"
    (let [h (harness {:pattern-candidates (fn [_] (throw (ex-info "index missing" {})))})
          {:keys [id]} (turn! h {:text "Please continue with the port." :dispatch :now})]
      (is (= 1 (count @(:bells h))))
      (is (not (str/includes? (:prompt (first @(:bells h))) "PRECOMPUTED")))
      (is (re-find #"pattern candidates failed" (:detail (svc/health (:svc h)))))
      (is (nil? (ts/read-pattern-candidates (get-in (:svc h) [:config :store]) id))))))

(deftest the-draft-is-tried-inline-with-an-off-path-backstop
  ;; M-象-2000: the draft is tried inline (bounded), so a :later turn usually
  ;; answers with it; dispatch! still drafts a record that has none (the
  ;; slow-draft timeout path), so the routine check still sees one.
  (let [drafted (atom 0)
        h (harness {:draft (fn [_] (swap! drafted inc) nil)
                    :draft-async! nil :evidence-async! (fn [t] (t))})
        {:keys [id]} (turn! h)]
    (is (= 1 @drafted) "the draft ran inline inside the bound")
    (is (= [] (delays h)) "nothing scheduled: the inline draft finished")
    (svc/dispatch! (:svc h) id {})
    (is (= 2 @drafted) "dispatch drafted the record that still had no draft")))

;; M-象-2000 step 2: one evidence entry per turn, once it has settled.

(deftest an-in-flight-turn-is-not-evidence
  (let [appended (atom [])
        h (harness {:evidence! (fn [e] (swap! appended conj e) {:ok true})})
        {:keys [id]} (turn! h)]
    (svc/attach-happened! (:svc h) id "did things")
    (is (empty? @appended) "recorded and dispatched, not yet read: nothing written")))

(deftest an-analysed-turn-is-one-entry-with-reading-and-happened
  ;; Reading first, happened second: the entry is written when the second
  ;; of the two arrives, and carries both.
  (let [appended (atom [])
        h (harness {:evidence! (fn [e] (swap! appended conj e) {:ok true})})
        {:keys [id record]} (turn! h)]
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (is (empty? @appended) "the reply has not ended: the write waits, bounded")
    (is (= [1800] (delays h)))
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (= 1 (count @appended)) "happened on a settled turn writes the packet")
    (let [e (first @appended)]
      (is (= (str "e-xiang-turn-" id) (:evidence-id e)))
      (is (= "emacs-abc" (:in-reply-to e)) "cites the operator turn")
      (is (= "claude-17-turn-3" (get-in e [:body :record :turn_id])))
      (is (= "analyzed" (get-in e [:body :reading :status])))
      (is (= "analyzed" (get-in e [:body :settled])))
      (is (some? (get-in e [:body :record :happened_summary])) "carries the summary")
      (is (some #{:xiang-turn-analyzed} (:tags e)))
      (is (= "sess-1" (:session-id e))))))

(deftest an-analysed-turn-posts-classical-scope-hyperedges
  (let [edges (atom [])
        h (harness {:scope-async! (fn [f] (f))
                    :post-hyperedge! (fn [edge]
                                       (swap! edges conj edge)
                                       {:status 201})})
        {:keys [id record]} (turn! h)
        _ (svc/publish-analysis! (:svc h) id (analysis-for record) {})]
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (seq @edges) "settling an analyzed turn emits its scope links")
    (is (every? #(and (= "xiang/scope" (:hx/type %))
                      (= 2 (count (:hx/endpoints %)))
                      (= "rule" (get-in % [:hx/props :labeller]))
                      (= "proposed" (get-in % [:hx/props :status]))
                      (str/starts-with? (get-in % [:hx/props :label-set])
                                        "scope-rules/"))
                @edges))))

(deftest scope-hyperedge-duplicates-and-failures-do-not-block-settlement
  (let [calls (atom 0)
        h (harness {:scope-async! (fn [f] (f))
                    :post-hyperedge! (fn [_]
                                       (swap! calls inc)
                                       {:status 409})})
        {:keys [id record]} (turn! h)]
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (pos? @calls) "a known hyperedge is still submitted")
    (is (= "analyzed" (:analysis_status (ts/read-record (get-in (:svc h) [:config :store]) id))))
    (let [failed (harness {:scope-async! (fn [f] (f))
                    :post-hyperedge! (fn [_] (throw (ex-info "store down" {})))})
          {:keys [id record]} (turn! failed)]
      (svc/publish-analysis! (:svc failed) id (analysis-for record) {})
      (is (= "analyzed" (:analysis_status (ts/read-record (get-in (:svc failed) [:config :store]) id)))))))

(deftest happened-then-reading-is-also-one-entry-with-both
  (let [appended (atom [])
        h (harness {:evidence! (fn [e] (swap! appended conj e) {:ok true})})
        {:keys [id record]} (turn! h)]
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (empty? @appended) "dispatched, not yet read: nothing written")
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (is (= 1 (count @appended)))
    (is (some? (get-in (first @appended) [:body :record :happened_summary])))
    (is (= "analyzed" (get-in (first @appended) [:body :settled])))))

(deftest a-turn-whose-reply-never-ends-is-written-after-the-bounded-wait
  (let [appended (atom [])
        h (harness {:evidence! (fn [e] (swap! appended conj e) {:ok true})})
        {:keys [id record]} (turn! h)]
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (is (empty? @appended))
    (is (= [1800] (delays h)) "the bounded wait for the summary")
    (run-next! h)
    (is (= 1 (count @appended)))
    (is (nil? (get-in (first @appended) [:body :record :happened_summary]))
        "written without the summary")
    (is (= "analyzed" (get-in (first @appended) [:body :settled])))))

(deftest a-routine-turn-is-one-entry-with-its-draft
  (let [appended (atom [])
        h (harness {:draft fake-draft :skip-routine? true
                    :evidence! (fn [e] (swap! appended conj e) {:ok true})})
        {:keys [id]} (turn! h {:text "Looks good." :dispatch :now})]
    (is (empty? @appended) "no reply yet: the write waits")
    (svc/attach-happened! (:svc h) id {:reply "done" :commits []})
    (is (= [(str "e-xiang-turn-" id)] (map :evidence-id @appended)))
    (is (= "drafted" (get-in (first @appended) [:body :settled])))
    (is (some? (get-in (first @appended) [:body :reading])))
    (is (some? (get-in (first @appended) [:body :record :happened_summary])))))

(deftest a-dark-turn-is-never-evidence
  ;; 象-off: not-requested turns go dark, and stay out of the database.
  (let [appended (atom [])
        h (harness {:evidence! (fn [e] (swap! appended conj e) {:ok true})})
        {:keys [id]} (turn! h {:analysis-requested? (constantly false)})]
    (svc/attach-happened! (:svc h) id "did things")
    (svc/dispatch! (:svc h) id {})
    (is (empty? @appended))))

(deftest a-retried-append-is-a-quiet-duplicate
  ;; The bad case idempotence is named for: the same settled turn twice.
  (let [seen (atom [])
        h (harness {:evidence! (fn [e]
                                 (if (some #{(:evidence-id e)} @seen)
                                   {:ok false :error/code :duplicate-id :idempotent? true}
                                   (do (swap! seen conj (:evidence-id e)) {:ok true})))})
        {:keys [id record]} (turn! h)
        _ (svc/publish-analysis! (:svc h) id (analysis-for record) {})
        _ (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
        result (svc/append-evidence! (:svc h) (svc/turn-evidence-entry id record {} :analyzed) [60])]
    (is (:idempotent? result))
    (is (= 1 (count @seen)) "still exactly one entry")
    (is (empty? (filter #(= 60 %) (delays h))) "a duplicate never schedules a retry")
    (is (not= :failing (:state (svc/health (:svc h)))) "a duplicate never fails health")))

(deftest a-failed-append-never-loses-the-turn
  (let [attempts (atom 0)
        h (harness {:evidence! (fn [_] (swap! attempts inc) (throw (ex-info "futon1b busy" {})))
                    :store-busy-delays [60]})
        {:keys [id record]} (turn! h)
        out (svc/publish-analysis! (:svc h) id (analysis-for record) {})]
    (is (= "analyzed" (get-in out [:analysis :status])) "the reading still publishes")
    (is (zero? @attempts) "the write waits for the reply to end")
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (= 1 @attempts))
    (is (some #{60} (delays h)) "retried on the bounded schedule")))

(deftest a-missing-reply-parent-keeps-the-entry
  ;; The boundary refuses an in-reply-to it cannot resolve (:reply-not-found).
  (let [appended (atom [])
        h (harness {:evidence! (fn [e]
                                 (if (:in-reply-to e)
                                   {:ok false :error/code :reply-not-found}
                                   (do (swap! appended conj e) {:ok true})))})
        {:keys [id record]} (turn! h)]
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (= [(str "e-xiang-turn-" id)] (map :evidence-id @appended)))
    (is (some #{:xiang-reply-parent-missing} (:tags (first @appended))))
    (is (= "emacs-abc" (get-in (first @appended) [:body :record :evidence_id])))))

(deftest the-append-is-off-the-callers-path
  (let [appended (atom [])
        h (harness {:evidence! (fn [e] (swap! appended conj e) {:ok true})
                    :evidence-async! nil})
        {:keys [id record]} (turn! h)]
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (empty? @appended) "publish and happened returned before the append ran")
    (let [i (.indexOf (delays h) 0)]
      (is (<= 0 i))
      ((second (nth @(:scheduled h) i))))
    (is (= 1 (count @appended)))))

(deftest happened-inside-the-soon-window-does-not-dispatch-twice
  ;; Reply ends before the scheduled dispatch has drafted and belled: the
  ;; turn has no job id yet. Planted: count bells.
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :soon})]
    (is (empty? @(:bells h)) "nothing belled at send")
    (is (= :dispatch-scheduled (:reason (svc/attach-happened! (:svc h) id "did things"))))
    (run-next! h)
    (is (= 1 (count @(:bells h))) "exactly one dispatch")))

(deftest a-stranded-soon-mark-still-lets-the-turn-go
  ;; The JVM restarted between the mark and the dispatch: after 5 minutes the
  ;; reply end dispatches the turn itself.
  (let [h (harness)
        {:keys [id]} (turn! h {:dispatch :soon})]
    (reset! (:scheduled h) [])
    (swap! (:clock h) + (* 6 60 1000))
    (svc/attach-happened! (:svc h) id "did things")
    (is (= 1 (count @(:bells h))))))


;; ---------------------------------------------------------------------------
;; Ports: the HAPPENED / DIDN'T HAPPEN heads-up display (E-agency-work-orders).
;; The session is the turn-acts fixture: claude-17 turns 391-394 of session
;; 564c8e50, where the "yes 1"/"yes 2" offer was accepted in 393 and the
;; ask-actions of 391 and 394 were never answered.

(def ^:private ports-fixture-dir "test/futon3c/xiang/turn_acts_fixtures")
(def ^:private ports-fixture-ids ["turn-455grB" "turn-xJ4TAP" "turn-I7n9AZ" "turn-HUylGP"])

(defn- seed-ports-store
  "A temp store holding the four real settled turns 391-394 (copied
   unmodified), each carrying its reply text."
  ([] (seed-ports-store ports-fixture-ids))
  ([fixture-ids]
  (let [store (temp-store)]
    (.mkdirs (io/file (:dir store)))
    (doseq [fid fixture-ids]
      (io/copy (io/file ports-fixture-dir (str fid ".json"))
               (io/file (:dir store) (str fid ".json")))
      (io/copy (io/file ports-fixture-dir (str fid ".json.analysis.json"))
               (io/file (:dir store) (str fid ".json.analysis.json")))
      (ts/update-record! store fid
                         #(-> %
                              (assoc :analysis_status "analyzed"
                                     :reply_text (slurp (io/file ports-fixture-dir (str fid ".reply.txt"))))
                              (dissoc :ports :acts))))
    store)))

(deftest compute-ports-session-cap-is-call-scoped
  (let [store (seed-ports-store (take 3 ports-fixture-ids))
        h (harness {:store store})
        oldest (first ports-fixture-ids)]
    (svc/compute-ports! (:svc h) oldest {:session-cap 1})
    (is (str/includes? (or (:detail (svc/health (:svc h))) "") "No acts for turn"))
    (is (nil? (:ports (ts/read-record store oldest))))
    (is (nil? (:acts (ts/read-record store oldest))))
    (is (nil? (:standings (ts/read-record store oldest))))
    (is (nil? (:obligations (ts/read-record store oldest))))
    (is (= {:state "unknown" :reason "no-acts"
            :computed_at "1970-01-01T00:16:40Z"
            :rules_version turn-acts/rules-version}
           (:ports_status (ts/read-record store oldest))))
    (svc/compute-ports! (:svc h) oldest {:session-cap 10})
    (let [computed (:ports (ts/read-record store oldest))]
      (is (some? computed))
      (svc/compute-ports! (:svc h) oldest {:session-cap 1})
      (let [record (ts/read-record store oldest)]
        (is (= computed (:ports record)) "a later refusal preserves the computed value")
        (is (= ["unknown" "no-acts"]
               ((juxt :state :reason) (:ports_status record))))))))

(deftest session-end-consumption-is-idempotent-and-advances-cursor
  (let [store (temp-store)
        h (harness {:store store
                    :agent-roster (fn [agent-id]
                                    (when (= "agent" agent-id)
                                      {:agent/metadata {:mission-id "M-ended"}}))})
        transition {:id "session-end:agent:session:2026-10-11T10:00:00Z"
                    :agent "agent" :old_session "session" :new_session nil
                    :reason "reset" :at "2026-10-11T10:00:00Z"}]
    (.mkdirs (io/file (:dir store)))
    (ts/write-record-id!
     store "turn-seed"
     {:turn_id "agent-turn-1" :session_id "session" :agent_id "agent"
      :created_at "2026-10-11T09:00:00Z" :analysis_status "analyzed"
      :reply_text "㊢ marked reply"
      :acts [{:id "agent-turn-1-f-0" :kind "ask-action" :author "operator"}
             {:id "agent-turn-1-r-0" :kind "ask-action" :author "agent"}]
      :ports {:still_open [{:act "agent-turn-1-f-0"}
                           {:act "agent-turn-1-r-0"}]}})
    (let [first (svc/consume-session-end! (:svc h) transition)
          second (svc/consume-session-end! (:svc h) transition)
          record (:record first)
          [envelope lapse] (:acts record)]
      (is (:created? first))
      (is (false? (:created? second)))
      (is (= (str "turn-end-" (subs (tr/sha256 (:id transition)) 0 12))
             (:id first)))
      (is (= {:origin "system" :analysis_status "declared" :agent_id "agent"
              :session_id "session" :created_at "2026-10-11T10:00:00Z"
              :mission_id "M-ended"}
             (select-keys record [:origin :analysis_status :agent_id :session_id
                                  :created_at :mission_id])))
      (is (= {:id (:id transition) :reason "reset" :new_session nil}
             (:session_end record)))
      (is (= [1 "agent-turn-1-r-0" "agent" "session"]
             [(:marked_reply_count envelope) (:target lapse) (:agent envelope) (:session envelope)]))
      (is (= (:id transition)
             (str/trim (slurp (str (registry/session-end-outbox-path) ".cursor")))))
      (is (= 1 (count @(:scheduled h))) "the duplicate schedules no recomputation"))))

(deftest start-replays-only-lines-after-session-end-cursor
  (let [h (harness)
        path (registry/session-end-outbox-path)
        one {:id "end-1" :agent "a" :old_session "s1" :new_session nil
             :reason "reset" :at "2026-10-11T10:00:00Z"}
        two {:id "end-2" :agent "a" :old_session "s2" :new_session "s3"
             :reason "replaced" :at "2026-10-11T11:00:00Z"}]
    (io/make-parents path)
    (spit path (str (json/write-str one) "\n" (json/write-str two) "\n"))
    (spit (str path ".cursor") "end-1\n")
    (try
      (svc/start! (:svc h))
      (let [store (get-in (:svc h) [:config :store])
            records (ts/list-records store :session-id "s2" :limit 10)]
        (is (= 1 (count records)))
        (is (empty? (ts/list-records store :session-id "s1" :limit 10)))
        (is (= "end-2" (str/trim (slurp (str path ".cursor"))))))
      (finally (svc/stop! (:svc h))))))

(deftest session-end-with-no-records-has-only-envelope
  (let [h (harness)
        result (svc/consume-session-end!
                (:svc h) {:id "empty-end" :agent "agent" :old_session "empty"
                          :new_session nil :reason "reap" :at "2026-10-11T12:00:00Z"})]
    (is (= 1 (count (get-in result [:record :acts]))))
    (is (= 0 (get-in result [:record :acts 0 :marked_reply_count])))
    (run-next! h)
    (is (= "computed" (:state (:ports_status
                                (ts/read-record (get-in (:svc h) [:config :store]) (:id result))))))))

(deftest session-end-lapses-agent-ports-and-marks-operator-ports
  (doseq [[reply? expected-mark] [[true "orphaned"] [false "unread"]]]
    (testing expected-mark
      (let [store (seed-ports-store)
            h (harness {:store store})
            last-id (last ports-fixture-ids)
            session-id (:session_id (ts/read-record store last-id))]
        (when-not reply?
          (doseq [id ports-fixture-ids]
            (ts/update-record! store id #(assoc % :reply_text ""))))
        (doseq [id ports-fixture-ids]
          (svc/compute-ports! (:svc h) id {:session-cap 10000}))
        (let [operator-ids (set (for [id ports-fixture-ids
                                      act (:acts (ts/read-record store id))
                                      :when (= "operator" (:author act))]
                                  (:id act)))
              result (svc/consume-session-end!
                      (:svc h) {:id (str "end-" expected-mark) :agent "claude-17"
                                :old_session session-id :new_session nil :reason "reset"
                                :at "2026-10-11T12:00:00Z"})]
          (run-next! h)
          (let [end-record (ts/read-record store (:id result))
                lapsed (set (map :act (:closed_this_turn (:ports end-record))))]
            (when reply?
              (is (seq lapsed) "the end turn closes at least one agent port"))
            (is (empty? (set/intersection operator-ids lapsed))
                "no operator-authored act is targeted by a lapse"))
          (svc/compute-ports! (:svc h) last-id {:session-cap 10000})
          (let [record (ts/read-record store last-id)
                marked (filter #(operator-ids (:act %)) (get-in record [:ports :still_open]))]
            (is (seq marked))
            (is (every? #(= expected-mark (:mark %)) marked))))))))

(deftest settle-computes-ports
  (let [store (seed-ports-store)
        h (harness {:store store})
        reply (slurp (io/file ports-fixture-dir "turn-HUylGP.reply.txt"))]
    ;; happened arrives after the reading: the turn is already analyzed.
    (svc/attach-happened! (:svc h) "turn-HUylGP" {:reply reply :commits []})
    (let [rec (ts/read-record store "turn-HUylGP")]
      (is (= reply (:reply_text rec)) "the reply text is stored for 大象")
      (let [{:keys [state reason computed_at rules_version]} (:ports_status rec)]
        (is (= "computed" state))
        (is (nil? reason))
        (is (= turn-acts/rules-version rules_version))
        (is (instance? java.time.Instant (java.time.Instant/parse computed_at))))
      (let [acts (:acts rec)
            allowed #{:id :kind :author :at :to :turn :record :port? :standing?
                      :target :carries-out :about :option :sha :repo :subject
                      :start :end :text}
            expected (->> (turn-acts/session-acts
                           (mapv (fn [fid]
                                   {:record-id fid
                                    :record (ts/read-record store fid)
                                    :reading (ts/read-analysis store fid)
                                    :reply (:reply_text (ts/read-record store fid))
                                    :commits (turn-acts/happened-commits
                                              (:happened_summary (ts/read-record store fid)))})
                                 ports-fixture-ids))
                          (filter #(= "turn-HUylGP" (:record %))))]
        (is (= (set (map :id expected)) (set (map :id acts))))
        (is (every? #(and (contains? % :port?) (contains? % :standing?)) acts))
        (is (every? #(every? allowed (keys %)) acts))
        (is (every? #(or (nil? (:text %)) (<= (count (:text %)) 120)) acts)))
      (let [ports (:ports rec)]
        (is (some? ports) "the settle computed ports")
        (is (contains? ports :inferred) "the stored ports carry inference witnesses")
        (is (= [] (:closed_this_turn ports)) "nothing closes in 394")
        (let [open (into {} (map (juxt :act identity) (:still_open ports)))]
          (is (= "ask-action" (get-in open ["claude-17-turn-391-r-3" :kind]))
              "391's unanswered ask-action is still open at 394")
          (is (str/starts-with? (get-in open ["claude-17-turn-391-r-3" :text])
                                "(one decision) Shall I read")
              "the HUD needs no lookup: the act carries its text")
          (is (= "2026-10-01T13:49:05Z" (get-in open ["claude-17-turn-391-r-3" :since])))
          (is (some #(str/includes? (:text %) "do 1 now and look into 2") (:still_open ports))
              "394's own ask-action is open at its turn")))
      (is (= #{:in_force :adopted_not_applied} (set (keys (:standings rec)))))
      (is (vector? (get-in rec [:standings :in_force])))
      (is (vector? (get-in rec [:standings :adopted_not_applied])))
      (is (vector? (:obligations rec))))
    ;; The accepting turn: its ports close the offer.
    (svc/compute-ports! (:svc h) "turn-I7n9AZ")
    (let [record (ts/read-record store "turn-I7n9AZ")
          ports (:ports record)
          commit (some #(when (= "d9d30ead" (:sha %)) %) (:acts record))]
      (is (= 1 (count (:closed_this_turn ports))))
      (is (= "offer" (:kind (first (:closed_this_turn ports)))))
      (is (str/includes? (:text (first (:closed_this_turn ports))) "yes 1"))
      (is (= {:sha "d9d30ead" :repo "futon3c"
              :subject "P11 acceptance: accept '🈸:yes' as a spelling of yes in both parsers (+23 -6 over 3 files)"
              :carries-out "claude-17-turn-392-r-4"}
             (select-keys commit [:sha :repo :subject :carries-out]))))))

(deftest persisted-acts-use-record-identity-when-turn-ids-repeat
  (let [store (seed-ports-store (take 2 ports-fixture-ids))
        h (harness {:store store})
        first-id (first ports-fixture-ids)
        second-id (second ports-fixture-ids)
        reused-turn (:turn_id (ts/read-record store first-id))]
    (ts/update-record! store second-id #(assoc % :turn_id reused-turn))
    (svc/compute-ports! (:svc h) second-id {:session-cap 10})
    (let [acts (:acts (ts/read-record store second-id))]
      (is (seq acts))
      (is (every? #(= second-id (:record %)) acts))
      (is (every? #(str/includes? (:id %) (str "@" second-id)) acts))
      (is (not-any? #(= first-id (:record %)) acts)))))

(deftest ports-land-in-either-arrival-order
  (testing "reading first, happened second"
    (let [h (harness)
          {:keys [id record]} (turn! h)
          store (get-in (:svc h) [:config :store])]
      (svc/publish-analysis! (:svc h) id (analysis-for record) {})
      (is (nil? (:ports (ts/read-record store id))) "no happened yet: no ports")
      (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
      (is (some? (:ports (ts/read-record store id))) "happened after reading recomputes")))
  (testing "happened first, reading second"
    (let [h (harness)
          {:keys [id record]} (turn! h)
          store (get-in (:svc h) [:config :store])]
      (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
      (is (nil? (:ports (ts/read-record store id))) "not yet read: no ports")
      (svc/publish-analysis! (:svc h) id (analysis-for record) {})
      (is (some? (:ports (ts/read-record store id))) "the reading settles into ports"))))

(deftest a-ports-failure-never-fails-the-turn
  ;; The exact bad case the guard is named for: the adapter throws (a
  ;; settled sibling record whose created_at no Instant parses), and the
  ;; turn still settles.
  (let [appended (atom [])
        h (harness {:evidence! (fn [e] (swap! appended conj e) {:ok true})})
        {:keys [id record]} (turn! h)
        store (get-in (:svc h) [:config :store])
        {bad-id :id} (turn! h {:turn-id "claude-17-turn-4"})]
    (svc/publish-analysis! (:svc h) bad-id (analysis-for (ts/read-record store bad-id)) {})
    (ts/update-record! store bad-id #(assoc % :created_at "not-a-time"))
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    (is (= 1 (count @appended)) "the settle packet still lands")
    (is (= "analyzed" (:analysis_status (ts/read-record store id))) "the turn still settles")
    (is (nil? (:ports (ts/read-record store id))))
    (is (str/includes? (or (get-in (svc/health (:svc h)) [:detail]) "") "ports not computed")
        "the failure is health detail only")))

(deftest an-operator-turn-records-what-it-answers
  ;; Joe points with `<mark>:`; the agent's newest turn since his last one
  ;; (here one that answered a bellback) carries that mark.
  (let [h (harness {:recent-agent-turns
                    (fn [agent]
                      (is (= "claude-17" agent))
                      [{:turn-id "t-nudge" :origin "work-orders"
                        :text "㊥ (gist) nudged.\n\n🈸 (still open) Shall I start?"}
                       {:turn-id "t-joe" :origin "operator"
                        :text "🈸 (next) Shall I do the other thing?"}])})
        {:keys [id]} (turn! h {:text "🈸: yes, go ahead.\n\n㊩ the lighter is still red."})
        rec (ts/read-record (get-in (:svc h) [:config :store]) id)]
    (is (= ["t-nudge"] (map :turn-id (get-in rec [:reply_to :replies]))))
    (is (= ["㊩"] (map :mark (get-in rec [:reply_to :declared_intents]))))))

(deftest a-failing-candidate-source-never-fails-the-record
  (let [h (harness {:recent-agent-turns (fn [_] (throw (ex-info "boom" {})))})
        {:keys [id]} (turn! h {:text "🈸: yes"})]
    (is (some? (ts/read-record (get-in (:svc h) [:config :store]) id)))))

(deftest quoted-material-never-reaches-the-file-xiang-reads
  ;; Joe (2026-10-04): a >>> block must not be passed to 象. 象 is pointed at
  ;; the record file, so neither the record's text fields nor anything else
  ;; in it may carry the quote; the feed reads it from a sidecar.
  (let [secret "the long quoted passage only codex-10 should read"
        typed (str "Please look at this.\n>>>\n" secret "\n>>>\nWhat do you think?")
        h (harness)
        {:keys [id]} (turn! h {:text typed :original-text typed})
        store (get-in (:svc h) [:config :store])
        raw (slurp (ts/record-path store id) :encoding "UTF-8")]
    (is (not (str/includes? raw secret)) "the record file never holds the quote")
    (is (str/includes? (:source_text (ts/read-record store id)) "QUOTE"))
    (is (= 1 (:quote_count (ts/read-record store id))))
    (is (= [secret] (json/read-str (slurp (ts/quotes-path store id)))))
    (let [brief (tr/analysis-brief id (ts/record-path store id) {})]
      (is (not (str/includes? brief secret)))
      (is (not (str/includes? brief ".quotes.json")) "the brief never names the sidecar"))))


(deftest a-first-turn-takes-the-agent-session-at-reply-end
  (testing "the 2026-10-04 demo: claude-5's first turn was recorded under
            the buffer's placeholder and so sat in a session of its own"
    (let [h (harness)
          store (get-in (:svc h) [:config :store])
          {:keys [id]} (turn! h {:session-id "claude-5 (awaiting session)" :agent-id "claude-5"})
          sessions {"claude-5" "b52510f9-275c-44e1-a74f-243df3750115"}]
      (svc/attach-happened! (:svc h) id {:reply "done" :commits []} {:agent-session sessions})
      (is (= "b52510f9-275c-44e1-a74f-243df3750115" (:session_id (ts/read-record store id))))))
  (testing "a real session is never replaced, and no session leaves the placeholder"
    (let [h (harness)
          store (get-in (:svc h) [:config :store])
          {id1 :id} (turn! h {:session-id "sess-1"})
          {id2 :id} (turn! h {:session-id "claude-5 (awaiting session)" :agent-id "claude-5"
                              :turn-id "claude-5-turn-1"})]
      (svc/attach-happened! (:svc h) id1 {:reply "done" :commits []} {:agent-session (constantly "other")})
      (svc/attach-happened! (:svc h) id2 {:reply "done" :commits []} {:agent-session (constantly nil)})
      (is (= "sess-1" (:session_id (ts/read-record store id1))))
      (is (= "claude-5 (awaiting session)" (:session_id (ts/read-record store id2)))))))

(deftest a-reading-published-by-the-script-is-settled-at-reap
  (testing "the 象 seats publish with session_turn_analysis.py (file plus
            flag, no JVM call); until 2026-10-04 such turns never got
            ports or settled evidence. The reap now settles them."
    (let [appended (atom [])
          h (harness {:evidence! (fn [e] (swap! appended conj (:evidence-id e)) {:ok true})})
          {:keys [id record]} (turn! h)
          store (get-in (:svc h) [:config :store])]
      (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
      ;; What complete() in session_turn_analysis.py does:
      (spit (ts/analysis-path store id) (json/write-str (assoc (analysis-for record) :status "analyzed")))
      (ts/update-record! store id #(assoc % :analysis_status "analyzed"))
      (is (nil? (:ports (ts/read-record store id))))
      (is (= :analyzed (:outcome (svc/reap! (:svc h) id {}))))
      (is (some? (:ports (ts/read-record store id))) "ports computed at reap")
      (is (some #{(str "e-xiang-turn-" id)} @appended) "the settled turn is written as evidence"))))

(deftest a-published-reading-with-a-stale-flag-is-flagged-at-reap-and-settles-silently-after
  ;; Live 2026-10-16: nine turns had the analysis file but the flag still
  ;; `requested`. Every GET of the turn view reaped them again (the Emacs
  ;; poll, every 20 s), each reap re-settled, each settle appended a
  ;; duplicate evidence entry and fired the turn listeners, and the frames
  ;; cache rebuilt back to back.
  (let [appended (atom []) events (atom [])
        h (harness {:evidence! (fn [e]
                                 (let [dup? (some #{(:evidence-id e)} @appended)]
                                   (swap! appended conj (:evidence-id e))
                                   (if dup? {:ok true :idempotent? true} {:ok true})))})
        {:keys [id record]} (turn! h {:dispatch :none})
        store (get-in (:svc h) [:config :store])]
    (svc/attach-happened! (:svc h) id {:reply "did things" :commits []})
    ;; The seat was asked (the live records carried `requested` and a job id)
    ;; and complete() wrote the file and, this time, not the flag.
    (ts/update-record! store id #(assoc % :analysis_status "requested"))
    (spit (ts/analysis-path store id) (json/write-str (assoc (analysis-for record) :status "analyzed")))
    (reset! (:scheduled h) [])
    (svc/add-turn-listener! ::test #(swap! events conj (:event %)))
    (try
      (is (= "requested" (:analysis_status (ts/read-record store id))))
      (is (= {:outcome :analyzed :status "analyzed"} (svc/reap! (:svc h) id {})))
      (while (seq @(:scheduled h)) (run-next! h))
      (is (= "analyzed" (:analysis_status (ts/read-record store id)))
          "the reap writes the flag the script omitted")
      (is (= [:reading :settled] @events))
      (is (= 1 (count @appended)))
      ;; A second reap (forced; the view no longer reaps a flagged record)
      ;; re-settles the same reading: the append is the idempotent duplicate
      ;; and no listener hears of it.
      (is (= :analyzed (:outcome (svc/reap! (:svc h) id {}))))
      (while (seq @(:scheduled h)) (run-next! h))
      (is (= [:reading :settled] @events) "a repeat settle of the same reading is silent")
      (is (= 2 (count @appended)))
      (finally (svc/remove-turn-listener! ::test)))))
