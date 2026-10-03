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

(deftest a-recorded-turn-and-its-reading-become-evidence
  ;; M-象-2000 step 2: one entry for the record (citing the operator turn's
  ;; evidence_id), one for the published reading (citing the record's entry).
  (let [appended (atom [])
        h (harness {:evidence! (fn [entry] (swap! appended conj entry) {:ok true})})
        {:keys [id record]} (turn! h)]
    (is (= 1 (count @appended)))
    (let [entry (first @appended)]
      (is (= (str "e-xiang-turn-" id) (:evidence-id entry)))
      (is (= {:ref/type :thread :ref/id id} (:subject entry)))
      (is (= "emacs-abc" (:in-reply-to entry)) "cites the operator turn's evidence id")
      (is (= record (:body entry)))
      (is (= "sess-1" (:session-id entry)))
      (is (some #{:xiang-turn-record} (:tags entry))))
    (svc/publish-analysis! (:svc h) id (analysis-for record) {})
    (is (= 2 (count @appended)))
    (let [entry (second @appended)]
      (is (= (str "e-xiang-reading-" id) (:evidence-id entry)))
      (is (= (str "e-xiang-turn-" id) (:in-reply-to entry)) "cites the record's entry")
      (is (= {:ref/type :thread :ref/id id} (:subject entry)))
      (is (some #{:xiang-turn-reading} (:tags entry))))))

(deftest a-retried-evidence-append-is-a-quiet-duplicate
  ;; The bad case idempotence is named for: the effect runs twice for the
  ;; same record id. The second append answers duplicate-id; it must not
  ;; schedule a retry, must not touch health, and the store sees one entry.
  (let [seen (atom [])
        h (harness {:evidence! (fn [entry]
                                 (if (some #{(:evidence-id entry)} @seen)
                                   {:ok false :error/code :duplicate-id :idempotent? true}
                                   (do (swap! seen conj (:evidence-id entry))
                                       {:ok true})))})
        {:keys [id record]} (turn! h)]
    (is (= [(str "e-xiang-turn-" id)] @seen))
    (let [result (svc/append-evidence! (:svc h) (svc/turn-evidence-entry id record)
                                       [60 180 600])]
      (is (false? (:ok result)))
      (is (:idempotent? result)))
    (is (= [(str "e-xiang-turn-" id)] @seen) "still exactly one entry")
    (is (empty? @(:scheduled h)) "a duplicate never schedules a retry")
    (is (nil? (:state (svc/health (:svc h)))) "a duplicate never touches health")))

(deftest a-failed-evidence-append-never-blocks-the-turn
  ;; futon1b throws: the turn is still recorded, the reading still
  ;; published, the append is retried once on the bounded schedule, and
  ;; health says the file is the source of truth.
  (let [attempts (atom 0)
        h (harness {:evidence! (fn [_] (swap! attempts inc)
                                 (throw (ex-info "futon1b busy" {})))
                    :store-busy-delays [60]})
        {:keys [id record]} (turn! h)]
    (is (some? (ts/read-record (get-in (:svc h) [:config :store]) id))
        "the turn is recorded even while evidence is down")
    (is (= 1 @attempts))
    (is (= [60] (delays h)) "the failure is retried on the bounded schedule")
    (run-next! h)
    (is (= 2 @attempts) "the retry ran the effect again")
    (is (= :failing (:state (svc/health (:svc h)))))
    (is (re-find #"file record is the source of truth"
                 (:detail (svc/health (:svc h)))))
    (let [out (svc/publish-analysis! (:svc h) id (analysis-for record) {})]
      (is (= "analyzed" (get-in out [:analysis :status])) "the reading still publishes")
      (is (ts/analysis-published? (get-in (:svc h) [:config :store]) id)))
    (is (= 3 @attempts))))

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
    (is (true? draft))
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
