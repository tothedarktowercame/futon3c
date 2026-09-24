(ns futon3c.agency.clock-decision-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.clock-decision :as decision]
            [futon3c.agency.clock-store :as clock]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.codex-cli :as codex-cli]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as store]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(use-fixtures :each
  (fn [f]
    (clock/reset-store!)
    (reg/reset-registry!)
    (with-redefs [bb/project-agents! (constantly nil)
                  reg/ring-bell-file! (constantly nil)]
      (try (f) (finally (clock/reset-store!) (reg/reset-registry!))))))

(defn- context [id]
  {:agent-id "clock-worker" :session-id "clock-session" :turn-id id
   :surface "bell" :phase :accepted :text "Please continue the work."})

(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:error/code (ex-data e)))))

(deftest volatile-production-store-is-refused
  (binding [decision/*test-store* nil]
    (is (= :clock/non-durable-backend
           (failure #(decision/record! (assoc (context "bad") :evidence-store (atom {}))))))))

(deftest no-source-and-idempotent-retry
  (let [backend (atom {:entries {} :order []})]
    (binding [decision/*test-store* backend decision/*repo-roots* {}]
      (let [first (decision/record! (context "one"))]
        (is (= [:unclocked :no-source 4] ((juxt :status :reason :source) first)))
        (is (= first (decision/record! (context "one"))))
        (is (= 1 (count (store/query* backend {:query/tags [:clock-decision]}))))))))

(defn- register! [invoke]
  (reg/register-agent! {:agent-id {:id/value "clock-worker" :id/type :continuity}
                        :type :claude :session-id "clock-session"
                        :capabilities [:edit] :invoke-fn invoke}))

(deftest persistence-failure-prevents-agent-execution
  (let [called (atom false)]
    (register! (fn [& _] (reset! called true) {:result "wrong"}))
    (binding [decision/*test-store* nil]
      (let [result (reg/invoke-agent! "clock-worker" "continue"
                                     {:evidence-store (atom {})})]
        (is (false? (:ok result)))
        (is (= :clock/non-durable-backend (get-in result [:error :error/code])))
        (is (false? @called))))))

(defn- with-docs [f]
  ;; The canonical mission parser recognizes /home/joe/code paths. Put genuine
  ;; isolated fixture files there; no parser stub or fake path association.
  (let [root (.toFile (Files/createTempDirectory
                       (.toPath (io/file "/home/joe/code"))
                       "futon-clock-test-" (make-array FileAttribute 0)))
        source (io/file root "src" "work.clj")
        mission (io/file root "holes" "missions" "M-clock-fixture.md")]
    (try
      (.mkdirs (.getParentFile source))
      (.mkdirs (.getParentFile mission))
      (spit source "; fixture\n")
      (spit mission (str "# Mission: clock fixture\n\nCode: `" source "`\n"))
      (binding [decision/*repo-roots* {:fixture (str root)}]
        (f root source mission))
      (finally (doseq [file (reverse (file-seq root))] (io/delete-file file true))))))

(deftest exact-mention-overrides-stale-clock-and-ambiguity-refuses
  (with-docs
    (fn [root _ _]
      (let [backend (atom {:entries {} :order []})]
        (binding [decision/*test-store* backend]
          (clock/set-dispatch-mission! "clock-worker" "clock-session" "E-stale")
          (let [d (decision/record! (assoc (context "mention") :text "Do M-clock-fixture."))]
            (is (= 1 (:source d)))
            (is (= {:mission-id "M-clock-fixture" :campaign-id nil :excursion-id nil}
                   (clock/current-clock "clock-worker" "clock-session"))))
          (spit (io/file root "holes" "missions" "M-other.md") "# other")
          (is (= :ambiguous
                 (:reason (decision/record!
                           (assoc (context "ambiguous") :text "M-clock-fixture and M-other")))))
          (is (= :unresolvable-target
                 (:reason (decision/record!
                           (assoc (context "unknown") :mission-id "M-missing")))))
          (is (= 3 (count (store/query* backend {:query/tags [:clock-decision]})))))))))

(deftest ^:slow complete-session-decisions-roundtrip-real-backend
  (with-docs
    (fn [root source _]
      (let [start! (requiring-resolve 'futon1b-server/start-server!)
            stop! (requiring-resolve 'futon1b-server/stop-server!)
            server (start! {:store-dir (str (io/file root "substrate"))
                            :bind-host "127.0.0.1" :port 0})
            node @(var-get (requiring-resolve 'futon1b-server/!node))
            base (str "http://127.0.0.1:" (.getPort (.getAddress server)))
            backend (f1b/make-futon1b-backend base)]
        (try
          (binding [decision/*test-store* nil]
            ;; A real registered session, actual file write, and the exact
            ;; tool-detail consumer used by the Claude invoke feed. No manual
            ;; clock and no named target in the dispatched prompt.
            (register! (fn [_ sid]
                         (spit source "; edited by the fixture agent\n")
                         ;; Exercise a pre-reload warm callback: the demux
                         ;; thread lacks the invocation's dynamic bindings.
                         (binding [decision/*turn* nil]
                           (decision/record-tool-use!
                            "clock-worker" sid
                            {:id "edit-1" :name "Edit" :input {:file_path (str source)}}))
                         {:result "edited" :session-id sid}))
            (is (:ok (reg/invoke-agent! "clock-worker" "Please implement the change."
                                       {:turn-id "activity-turn" :surface "bell"
                                        :evidence-store backend})))
            (is (= "M-clock-fixture"
                   (:mission-id (clock/current-clock "clock-worker" "clock-session"))))
            (is (= "M-clock-fixture"
                   (get-in (reg/registry-status) [:agents "clock-worker" :mission-id])))
            (let [reconstructed (f1b/make-futon1b-backend base)
                  rows (store/query* reconstructed {:query/tags [:clock-decision]})
                  activity (first (filter #(= :activity (get-in % [:evidence/body :phase])) rows))]
              (is (= 2 (count rows)))
              (is (= 3 (get-in activity [:evidence/body :source])))
              (is (= (str source) (get-in activity [:evidence/body :evidence :path])))
              (is (= "activity-turn" (get-in activity [:evidence/body :turn-id])))
              (is (= "clock-session" (get-in activity [:evidence/body :session-id]))))
            ;; Independent empty session: every accepted surface has a durable
            ;; negative decision. Registry state must not leak the prior mission.
            (clock/reset-store!)
            (reg/update-agent! "clock-worker" :agent/session-id "empty-session")
            (reg/update-agent! "clock-worker" :agent/invoke-fn
                               (fn [_ sid] {:result "done" :session-id sid}))
            (doseq [surface ["bell" "whistle" "emacs-repl" "marimo"]]
              (is (:ok (reg/invoke-agent! "clock-worker" "Hello"
                                         {:turn-id surface :surface surface
                                          :evidence-store backend}))))
            (let [rows (store/query* (f1b/make-futon1b-backend base)
                                     {:query/tags [:clock-decision]})
                  absent (filter #(contains? #{"bell" "whistle" "emacs-repl" "marimo"}
                                              (get-in % [:evidence/body :turn-id])) rows)]
              (is (= 4 (count absent)))
              (is (every? #(= [:unclocked :no-source]
                              ((juxt :status :reason) (:evidence/body %))) absent)))
            ;; A stale carried REPL clock must lose to the newly named target.
            (reg/update-agent! "clock-worker" :agent/session-id "clock-session")
            (clock/set-dispatch-mission! "clock-worker" "clock-session" "E-stale")
            ;; Operator evidence uses the same real authority and resolver.
            (let [handler (http/make-handler {:evidence-store backend})
                  response (handler {:request-method :post :uri "/api/alpha/evidence"
                                     :body (json/generate-string
                                            {:evidence-id "operator-evidence"
                                             :subject {:ref/type "session" :ref/id "clock-session"}
                                             :type "coordination" :claim-type "question" :author "joe"
                                             :session-id "clock-session"
                                             :body {:event "chat-turn" :role "user"
                                                    :turn-id "clock-worker-turn-9"
                                                    :transport "emacs-claude-repl" :mission-id "E-stale"
                                                    :text "Now do M-clock-fixture"}})})]
              (is (= 201 (:status response)))
              (is (= "M-clock-fixture" (:mission-id (clock/current-clock "clock-worker" "clock-session")))))
            (let [rows (store/query* (f1b/make-futon1b-backend base) {:query/tags [:clock-decision]})
                  d (:evidence/body (first (filter #(= :operator (get-in % [:evidence/body :phase])) rows)))]
              (is (= 1 (:source d)))
              (is (= {:mission-id "M-clock-fixture" :campaign-id nil :excursion-id nil} (merge (clock/empty-clock) (:clock d))))))
          (finally (stop! server) (.close ^java.lang.AutoCloseable node)))))))

(deftest activity-ambiguity-and-current-clock-precedence
  (with-docs
    (fn [root source _]
      (binding [decision/*test-store* (atom {:entries {} :order []})]
        (spit (io/file root "holes" "missions" "M-shared.md")
              (str "# shared\n`" source "`\n"))
        (let [ctx (assoc (context "shared") :phase :activity :edited-path (str source))]
          (is (= :ambiguous (:reason (decision/record! ctx))))
          (is (= (clock/empty-clock) (clock/current-clock "clock-worker" "clock-session")))
          (clock/set-dispatch-mission! "clock-worker" "clock-session" "M-current")
          (let [d (decision/record! (assoc ctx :turn-id "current"))]
            (is (= 2 (:source d)))
            (is (= "M-current" (get-in d [:clock :mission-id])))))))))

(deftest replay-does-not-roll-clock-back
  (with-docs
    (fn [_ _ _]
      (binding [decision/*test-store* (atom {:entries {} :order []})]
        (let [old (context "old")]
          (decision/record! old)
          (decision/record! (assoc (context "new") :text "M-clock-fixture"))
          (is (= :unclocked (:status (decision/record! old))))
          (is (= "M-clock-fixture"
                 (:mission-id (clock/current-clock "clock-worker" "clock-session")))))))))

(deftest swallowed-callback-accounting-failure-fails-the-session
  (let [backend (atom {:entries {} :order []})]
    (binding [decision/*test-store* backend decision/*repo-roots* {}]
      (register! (fn [_ sid]
                   ;; Reproduce the pouch consumer's catch-and-continue behavior
                   ;; after clock storage becomes invalid during an admitted turn.
                   (binding [decision/*test-store* nil]
                     (try (decision/record-tool-use!
                           "clock-worker" sid
                           {:name "Edit" :id "lost" :input {:file_path "/tmp/lost.clj"}})
                          (catch Exception _ nil)))
                   {:result "apparently succeeded" :session-id sid}))
      (let [result (reg/invoke-agent! "clock-worker" "continue"
                                     {:turn-id "lost-activity" :evidence-store backend})]
        (is (false? (:ok result)))
        (is (= :clock/activity-decision-failed (get-in result [:error :error/code])))
        (is (= :unclocked
               (get-in (first (store/query* backend {:query/tags [:clock-decision]}))
                       [:evidence/body :status])))))))

(deftest recovery-orders-ties-and-keeps-sessions-separate
  (let [backend (atom {:entries {} :order []})
        at "2026-09-21T18:33:00Z"
        positive {:agent-id "clock-worker" :session-id "clock-session"
                  :decision-id "a" :decided-at at :status :clocked
                  :clock {:mission-id "M-before"}}
        negative (assoc positive :decision-id "z" :status :unclocked :reason :no-source
                                 :clock (clock/empty-clock))]
    (binding [decision/*test-store* backend]
      (doseq [d [negative positive (assoc positive :decision-id "other" :session-id "other")]]
        (store/append* backend {:evidence/id (:decision-id d)
                                :evidence/subject {:ref/type :agent :ref/id "clock-worker"}
                                :evidence/type :coordination :evidence/claim-type :step
                                :evidence/author "clock-worker" :evidence/at at
                                :evidence/session-id (:session-id d)
                                :evidence/tags [:clock-decision] :evidence/body d}))
      (is (= {:restored 2} (decision/restore! backend "clock-worker")))
      (is (= "z" (get-in (clock/current-state "clock-worker" "clock-session")
                          [:decision :decision-id])))
      (is (= "M-before" (:mission-id (clock/current-clock "clock-worker" "other"))))
      (clock/set-decision! "clock-worker" "clock-session" positive)
      (is (= (clock/empty-clock) (clock/current-clock "clock-worker" "clock-session"))))))

(deftest ^:slow decisions-restore-after-restart-through-http-and-roster
  (with-docs
    (fn [root _ _]
      (let [start! (requiring-resolve 'futon1b-server/start-server!)
            stop! (requiring-resolve 'futon1b-server/stop-server!)
            server (start! {:store-dir (str (io/file root "restore-substrate"))
                            :bind-host "127.0.0.1" :port 0})
            node @(var-get (requiring-resolve 'futon1b-server/!node))
            base (str "http://127.0.0.1:" (.getPort (.getAddress server)))
            backend (f1b/make-futon1b-backend base)
            request {:request-method :get :uri "/api/alpha/agent-clock"
                     :query-string "agent-id=clock-worker&session-id=clock-session"}
            read-clock (fn [handler] (json/parse-string (:body (handler request)) true))]
        (try
          (binding [decision/*test-store* nil]
            (register! (fn [_ sid] {:result "done" :session-id sid}))
            (let [positive (decision/record! (assoc (context "positive")
                                                   :text "M-clock-fixture"
                                                   :evidence-store backend))]
              (clock/reset-store!)
              (is (= (clock/empty-clock) (clock/current-clock "clock-worker" "clock-session")))
              ;; Rebuilding the configured HTTP client restores registered agents,
              ;; exactly the helper called after roster restore during bootstrap.
              (let [client (f1b/make-futon1b-backend base)
                    handler (http/make-handler {:evidence-store client})]
                (is (= "M-clock-fixture" (:mission-id (read-clock handler))))
                (is (= "M-clock-fixture"
                       (get-in (reg/registry-status) [:agents "clock-worker" :mission-id])))
                (is (= (:decision-id positive)
                       (get-in (clock/current-state "clock-worker" "clock-session")
                               [:decision :decision-id]))))
              ;; Warm the ordinary evidence cache; the next durable write is made
              ;; outside this backend so its cache invalidation cannot help restore.
              (store/query* backend {:query/author "clock-worker" :query/tags [:clock-decision]})
              (let [negative (assoc positive :decision-id "external-unclocked"
                                    :decided-at (str (java.time.Instant/now))
                                    :status :unclocked :reason :no-source :source 4
                                    :clock (clock/empty-clock))
                    write! (requiring-resolve 'futon1b-evidence/write-evidence!)
                    [status _] (write! node {:evidence/id (:decision-id negative)
                                            :evidence/subject {:ref/type :agent :ref/id "clock-worker"}
                                            :evidence/type :coordination :evidence/claim-type :step
                                            :evidence/author "clock-worker"
                                            :evidence/at (:decided-at negative)
                                            :evidence/session-id "clock-session"
                                            :evidence/tags [:clock-decision]
                                            :evidence/body negative})]
                (is (= 201 status))
                (clock/reset-store!)
                ;; Also prove stale roster metadata cannot resurrect the old clock.
                (reg/update-agent! "clock-worker" :agent/metadata {:mission-id "M-stale"})
                (let [client (f1b/make-futon1b-backend base)
                      handler (http/make-handler {:evidence-store client})]
                  (is (nil? (:mission-id (read-clock handler))))
                  (is (nil? (get-in (reg/registry-status) [:agents "clock-worker" :mission-id])))
                  (is (= [:unclocked :no-source "external-unclocked"]
                         ((juxt :status :reason :decision-id)
                          (:decision (clock/current-state "clock-worker" "clock-session")))))
                  (is (= :unclocked
                         (get-in (store/get-entry* client "external-unclocked")
                                 [:evidence/body :status])))))
              ;; A later reconnect need not have existed in the startup roster.
              ;; Admission itself restores before calculating source 2.
              (decision/record! (assoc (context "positive-again") :text "M-clock-fixture"
                                       :evidence-store backend))
              (clock/reset-store!)
              (let [next (decision/record! (assoc (context "after-restart") :evidence-store backend))]
                (is (= 2 (:source next)))
                (is (= "M-clock-fixture" (get-in next [:clock :mission-id]))))))
          (finally (stop! server) (.close ^java.lang.AutoCloseable node)))))))

(deftest recovery-subdivides-before-reading-and-refuses-partial-pages
  (let [backend (atom {:entries {} :order []})
        reads (atom [])
        expected {:query/author "clock-worker" :query/session-id "clock-session"
                  :query/tags [:clock-decision]}]
    (binding [decision/*test-store* backend]
      (with-redefs [store/count* (fn [_ q]
                                  (is (nil? (:query/tags q)))
                                  (is (true? (:query/include-ephemeral? q)))
                                  (if (or (:query/since q) (:query/before q)) 2 102401))
                    store/query* (fn [_ q]
                                   (swap! reads conj q)
                                   (is (false? f1b/*query-cache-enabled*))
                                   [])]
        (is (= {:restored 0} (decision/restore! backend "clock-worker" "clock-session")))
        (is (= 2 (count @reads)))
        (is (every? #(= expected (select-keys % (keys expected))) @reads))
        (is (= (:query/before (first @reads)) (:query/since (second @reads)))))
      (with-redefs [store/query* (fn [& _] (with-meta [] {:partial? true}))]
        (is (= :clock/recovery-incomplete
               (failure #(decision/restore! backend "clock-worker" "clock-session"))))))))

(deftest ^:slow dispatch-inheritance-through-real-job-path
  (with-docs
    (fn [root _ _]
      (spit (io/file root "holes" "missions" "M-override.md") "# override")
      (let [start! (requiring-resolve 'futon1b-server/start-server!)
            stop! (requiring-resolve 'futon1b-server/stop-server!)
            server (start! {:store-dir (str (io/file root "inherit-substrate"))
                            :bind-host "127.0.0.1" :port 0})
            node @(var-get (requiring-resolve 'futon1b-server/!node))
            backend (f1b/make-futon1b-backend
                     (str "http://127.0.0.1:" (.getPort (.getAddress server))))]
        (try
          (binding [decision/*test-store* nil]
            (with-redefs [http/invoke-jobs-store-path (constantly (str (io/file root "jobs.edn")))]
              (http/reset-invoke-jobs!)
              (register! (fn [_ sid] {:result "done" :session-id sid}))
              (reg/register-agent! {:agent-id "caller" :type :claude :session-id "caller-session"
                                    :capabilities [] :invoke-fn (fn [_ sid] {:result "done" :session-id sid})})
              (let [caller-context (assoc (context "caller-positive") :agent-id "caller"
                                          :session-id "caller-session" :text "M-clock-fixture"
                                          :evidence-store backend)
                    parent (decision/record! caller-context)
                    dispatch (fn [surface mission]
                               (let [request {:evidence-store backend :agent-id "clock-worker"
                                              :caller "caller" :surface surface :prompt "Continue"
                                              :mission-id mission}
                                     id (#'http/create-invoke-job! request)]
                                 [id request]))
                    run (fn [[id request]]
                          (is (:ok (:result (#'http/run-invoke-job! (assoc request :job-id id)))))
                          (:decision (clock/current-state "clock-worker"
                                                          (:agent/session-id (reg/get-agent "clock-worker")))))
                    queued (dispatch "bell" nil)]
                ;; Dispatch captures the clock before a later caller switch.
                (decision/record! (assoc caller-context :turn-id "caller-negative" :text "M-missing"))
                (let [d (run queued)]
                  (is (= :inherited (:source d)))
                  (is (= "M-clock-fixture" (get-in d [:clock :mission-id])))
                  (is (= {:caller-id "caller" :caller-decision-id (:decision-id parent)} (:evidence d))))
                (decision/record! (assoc caller-context :turn-id "caller-clocked-again"))
                (is (= "M-override" (get-in (run (dispatch "bell" "M-override")) [:clock :mission-id])))
                (is (= 1 (get-in (clock/current-state "clock-worker" "clock-session") [:decision :source])))
                (decision/record! (assoc caller-context :turn-id "caller-unresolvable" :text "M-missing"))
                (reg/update-agent! "clock-worker" :agent/session-id "empty-child")
                (is (= [:unclocked :no-source 4]
                       ((juxt :status :reason :source) (run (dispatch "bell" nil)))))
                ;; A clocked returning worker must not clock its original caller.
                (decision/record! (assoc (context "returning-worker") :session-id "empty-child"
                                         :text "M-clock-fixture" :evidence-store backend))
                (let [request {:evidence-store backend :agent-id "caller" :caller "clock-worker"
                               :surface "auto-bellback" :prompt "Done"}
                      id (#'http/create-invoke-job! request)]
                  (is (nil? (:inherited-clock (#'http/get-invoke-job id))))
                  (is (:ok (:result (#'http/run-invoke-job! (assoc request :job-id id)))))
                  (is (= [:unclocked :no-source 4]
                         ((juxt :status :reason :source)
                          (:decision (clock/current-state "caller" "caller-session"))))))
                (is (seq (store/query* (f1b/make-futon1b-backend (:base-url backend))
                                       {:query/tags [:clock-decision] :query/author "clock-worker"}))))))
          (finally (http/reset-invoke-jobs!) (stop! server) (.close ^java.lang.AutoCloseable node)))))))


(deftest ^:slow codex-exec-rollout-files-clock-a-complete-session
  (with-docs
    (fn [root source _]
      (let [start! (requiring-resolve 'futon1b-server/start-server!)
            stop! (requiring-resolve 'futon1b-server/stop-server!)
            server (start! {:store-dir (str (io/file root "codex-substrate"))
                            :bind-host "127.0.0.1" :port 0})
            node @(var-get (requiring-resolve 'futon1b-server/!node))
            backend (f1b/make-futon1b-backend
                     (str "http://127.0.0.1:" (.getPort (.getAddress server))))
            fixture (.getCanonicalPath (io/file "test/fixtures/codex/exec-apply-patch.jsonl"))]
        (try
          (binding [decision/*test-store* nil]
            (register!
             (fn [_ sid]
               ;; A real subprocess writes in the declared mission tree and emits
               ;; the captured Codex rollout. Tool-call JavaScript is never evaled.
               (let [result (codex-cli/run-codex-stream!
                             ["python3" "-c"
                              (str "import sys,pathlib; sys.stdin.readline(); "
                                   "pathlib.Path('src/work.clj').write_text('; codex edited\\n'); "
                                   "lines=pathlib.Path(sys.argv[1]).read_text().splitlines(); "
                                   "print('\\n'.join(lines+[lines[-1]]))")
                              fixture]
                             "Implement the work" {:cwd (str root)})]
                 (is (= 0 (:exit result)))
                 (is (nil? (:error-text result)))
                 {:result "done" :session-id sid})))
            (is (:ok (reg/invoke-agent! "clock-worker" "Implement the work"
                                       {:turn-id "codex-rollout-turn" :surface "bell"
                                        :evidence-store backend})))
            (is (= "; codex edited\n" (slurp source)))
            (let [reconstructed (f1b/make-futon1b-backend (:base-url backend))
                  rows (store/query* reconstructed {:query/author "clock-worker"
                                                     :query/tags [:clock-decision]})
                  activities (filter #(= :activity (get-in % [:evidence/body :phase])) rows)
                  activity (:evidence/body (first activities))]
              (is (= 2 (count rows)))
              (is (= 1 (count activities)) "duplicate receipt is idempotent")
              (is (= 3 (:source activity)))
              (is (= "M-clock-fixture" (get-in activity [:clock :mission-id])))
              (is (= (str source) (get-in activity [:evidence :path])))
              (is (= "M-clock-fixture"
                     (get-in (reg/registry-status) [:agents "clock-worker" :mission-id])))))
          (finally (stop! server) (.close ^java.lang.AutoCloseable node)))))))


(deftest top-level-targets-share-ingestion-rule-and-duplicates-refuse
  (with-docs
    (fn [root _ _]
      (let [top (io/file root "holes" "M-foo.md")
            nested (io/file root "holes" "missions" "M-bar.md")
            ignored (io/file root "holes" "archive" "M-hidden.md")]
        (spit top "# top-level mission")
        (spit nested "# nested mission")
        (.mkdirs (.getParentFile ignored))
        (spit ignored "# outside mission directories")
        (binding [decision/*test-store* (atom {:entries {} :order []})]
          (doseq [id ["M-foo" "M-bar"]]
            (let [d (decision/record! (assoc (context id) :text id))]
              (is (= 1 (:source d)))
              (is (= id (get-in d [:clock :mission-id])))))
          (is (= :unresolvable-target
                 (:reason (decision/record! (assoc (context "hidden") :text "M-hidden")))))
          ;; Adding a duplicate also exercises catalog cache invalidation.
          (spit (io/file root "holes" "missions" "M-foo.md") "# duplicate")
          (let [d (decision/record! (assoc (context "duplicate") :text "M-foo"))]
            (is (= [:unclocked :ambiguous 4] ((juxt :status :reason :source) d)))
            (is (= (clock/empty-clock) (clock/current-clock "clock-worker" "clock-session")))))))))

(deftest ^:slow real-futon2-root-resolves-operator-as-attached-agent
  ;; Explicit authority: unconfigured discovery also includes many worktrees.
  ;; The running JVM currently omits futon2 from FUTON3C_REPOS (reported).
  (binding [decision/*repo-roots* {:futon2 "/home/joe/code/futon2"}
            decision/*test-store* (atom {:entries {} :order []})]
    (is (.isFile (io/file "/home/joe/code/futon2/holes/E-operator-as-attached-agent.md")))
    (let [d (decision/record! (assoc (context "real-top-level-excursion")
                                   :mission-id "E-operator-as-attached-agent"))]
      (is (= [:clocked 1] ((juxt :status :source) d)))
      (is (= "E-operator-as-attached-agent" (get-in d [:clock :excursion-id]))))))

(deftest tickets-are-clock-targets
  ;; Joe (2026-09-24): a kimi requisition may name a ticket and its reminder
  ;; asks the caller to clock onto it, so T- docs resolve like M-/E-/C-.
  (with-docs
    (fn [root _ _]
      (let [nested (io/file root "holes" "tickets" "T-fix-thing.md")
            top (io/file root "holes" "T-top-ticket.md")]
        (.mkdirs (.getParentFile nested))
        (spit nested "# ticket")
        (spit top "# top-level ticket")
        (binding [decision/*test-store* (atom {:entries {} :order []})]
          (doseq [id ["T-fix-thing" "T-top-ticket"]]
            (let [d (decision/record! (assoc (context id)
                                             :surface "emacs-repl"
                                             :text (str "You requisitioned kimi-1 for " id ".")))]
              (is (= [:clocked 1] ((juxt :status :source) d)))
              (is (= {:campaign-id nil :mission-id nil :excursion-id nil :ticket-id id}
                     (clock/current-clock "clock-worker" "clock-session")))))
          (is (= :unresolvable-target
                 (:reason (decision/record! (assoc (context "missing") :text "T-missing")))))
          (is (= "T-fix-thing"
                 (get (do (decision/record! (assoc (context "again") :text "T-fix-thing"))
                          (clock/evidence-clock-fields "clock-worker" "clock-session"))
                      "clocked-ticket"))))))))
