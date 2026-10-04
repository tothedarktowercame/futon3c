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

(deftest a-soon-turn-is-answered-at-once-and-dispatched-off-the-path
  ;; The Emacs jvm recorder sends "soon": POST returns before the draft or
  ;; the bell runs, then dispatch! runs on the scheduler at delay 0.
  (let [drafted (atom 0)
        h (harness {:draft (fn [_] (swap! drafted inc) nil)})
        {:keys [id dispatch]} (turn! h {:dispatch :soon})]
    (is (= :scheduled dispatch))
    (is (zero? @drafted) "record-turn! returned before the draft ran")
    (is (empty? @(:bells h)) "and before the seat was belled")
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

(deftest the-draft-is-off-the-request-path
  ;; A slow draft (11 s live) must not hold POST /turns; dispatch! drafts a
  ;; record that still has none, so the routine check still sees one.
  (let [drafted (atom 0)
        h (harness {:draft (fn [_] (swap! drafted inc) nil)
                    :draft-async! nil :evidence-async! (fn [t] (t))})
        {:keys [id]} (turn! h)]
    (is (zero? @drafted) "record-turn! returned before the draft ran")
    (is (= [0] (delays h)))
    (svc/dispatch! (:svc h) id {})
    (is (= 1 @drafted) "dispatch drafted the record that had no draft yet")))

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

