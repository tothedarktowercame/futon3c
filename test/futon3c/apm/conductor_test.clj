(ns futon3c.apm.conductor-test
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.registry :as agency]
            [futon3c.apm.conductor :as conductor]
            [futon3c.apm.conductor-binding :as binding]
            [futon3c.apm.conductor-surface :as conductor-surface]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.tools :as tools]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute FileTime]))

(def ^:private registration-path
  "holes/labs/M-apm-demonstration/round1-registration.edn")

(def ^:private registration (edn/read-string (slurp registration-path)))
(def ^:private environment-revision (:reg/environment-revision registration))
(def ^:private harness-revision (:reg/harness-revision registration))

;; The frozen round-1 EDN predates the seat-key gate (:unstaffed-carded-seat,
;; merged with feat/registration-seat-keys) and must not be edited, so the
;; fixture stages a staffed copy under a temp path for the machine to read.
(def ^:private staffed-registration
  (assoc registration
         :reg/guide-seat "conductor-test"
         :reg/proctor-seat "proctor-test"
         :reg/scribe-seat "scribe-test"
         :reg/student-seat "zai-1"))

(defn- fixture []
  (let [state-root (.toFile
                    (Files/createTempDirectory
                     "conductor-state-" (make-array FileAttribute 0)))
        scaffold (Files/createTempFile "conductor-scaffold-" ".lean"
                                       (make-array FileAttribute 0))
        closing (Files/createTempFile "conductor-closing-" ".lean"
                                      (make-array FileAttribute 0))
        witness (Files/createTempFile "conductor-witness-" ".edn"
                                      (make-array FileAttribute 0))
        authorization (Files/createTempFile "conductor-authorization-" ".edn"
                                            (make-array FileAttribute 0))
        staffed-reg (Files/createTempFile "conductor-registration-" ".edn"
                                          (make-array FileAttribute 0))
        deposit-seq (atom 0)
        dispatch-fn
        (fn [opts _]
          {:ok true :job-id "job-test" :sent-opts opts
           :evidence {:body {:job-id "job-test"
                             :eligible-memory-ids ["memory/a" "memory/b"]
                             :memory-use
                             {:memory-use/surfaced-ids ["memory/a"]}}}})
        provisioner
        (fn [{:keys [arm branch batch]}]
          {:checkout (str "/tmp/conductor/" arm)
           :base-revision environment-revision
           :branch branch :frame/id (str batch "-" arm) :batch batch})
        peripheral
        (problem/make-problem
         (tools/make-mock-backend) dispatch-fn (.getPath state-root) provisioner
         (fn [_] {:harness-revision harness-revision
                  :harness-tree-dirty? false})
         (constantly ["memory/a" "memory/b"])
         (constantly 0)
         (fn [_ _]
           {:ok true :id (str "memory/deposit-" (swap! deposit-seq inc))}))]
    (spit (.toFile scaffold) "scaffold\n")
    (spit (.toFile closing) "closing\n")
    (spit (.toFile witness) "{:contained? true}\n")
    (spit (.toFile staffed-reg) (pr-str staffed-registration))
    (Files/setLastModifiedTime scaffold (FileTime/fromMillis 1000))
    (Files/setLastModifiedTime closing (FileTime/fromMillis 2000))
    {:config
     {:session-id "conductor-test" :problem-id "t94J02" :mode :store-mode
      :registration-path (str staffed-reg)
      :frame {:scaffold-path scaffold :closing-path closing
              :witness-path witness}
      :checkout {:batch "conductor-test" :base-rev environment-revision
                 :solver-seat "codex-4" :student-seat "zai-1"
                 :recall-system "futon1b"}
      :evidence-store (atom {:entries {} :order []})
      :harness-repo "/harness" :lean-repo "/lean"
      :agency-endpoint "http://127.0.0.1:1/unreachable"
      :authorization-revision (apply str (repeat 40 "a"))
      :authorization-output (str authorization)
      :conductor "conductor-test" :peripheral peripheral}
     :paths [scaffold closing witness authorization staffed-reg]}))

(defn- solver-attempt []
  {:attempt/id "attempt/solver" :attempt/seq 0
   :cycle/regime "round-1" :cycle/store-revision "store-1"
   :cycle/runner-freshness :cold})

(defn- student-attempt []
  {:attempt/id "attempt/student" :attempt/seq 1
   :cycle/regime "round-1" :cycle/store-revision "store-2"
   :cycle/runner-freshness :cold})

(deftest conductor-runs-a-refused-cycle-and-keeps-its-rider-ledger
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            missing-mission (conductor/dispatch-solver! opened {} "packet")
            solver (conductor/dispatch-solver! opened {:mission "M-test"} "packet")
            intervening (conductor/record-solver-attempt!
                         solver (solver-attempt) {})
            deposited (conductor/deposit!
                       intervening
                       {:name "deposit" :kind :feedback :hook "test"
                        :body {:lesson "ledger"}
                        :subjects [{:ref/type :problem :ref/id "t94J02"}]})
            student (conductor/dispatch-student!
                     deposited {:mission "M-test"} "student packet")
            adjudicating (conductor/record-students!
                          student [(student-attempt)] [])
            closing (conductor/adjudicate!
                     adjudicating
                     {:outcome :tier-a :residual-sorries 1 :axiom-clean? false
                      :promotion-result []})
            closed (conductor/close! closing)]
        (is (:ok opened) (pr-str (:error opened)))
        (is (= :guided-solve (get-in opened [:state :current-phase])))
        (is (= :mission-absent (get-in missing-mission [:error :error/code])))
        (is (= ["memory/deposit-1"] (:deposits deposited)))
        (is (nil? (get-in closed [:state :current-phase]))
            "the final advance reaches the terminal sentinel")
        (is (false? (get-in closed [:envelope :launchable?])))
        (is (seq (get-in closed [:envelope :failures]))
            "round-one closes with an honest refusal envelope")
        (is (= [{:offer/id "offer/job-test/0"
                 :offer/memory-id "memory/a"}]
               (mapv #(select-keys % [:offer/id :offer/memory-id])
                     (get-in closed [:state :cycle/outputs :memory-offers])))
            "the conductor converts the dispatch receipt into offer entities")
        (is (not-any? #{:malformed-memory-offers}
                      (get-in closed [:envelope :failures]))
            "conductor-collected receipts validate as memory-offer entities"))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest resume-loads-a-checkpoint-and-can-continue
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            version (count (filter #(= :problem-save (:tool %)) (:log opened)))
            resumed (conductor/resume opened (:cycle-id opened) version)
            continued (conductor/dispatch-solver!
                       resumed {:mission "M-test"} "continued packet")]
        (is (:ok opened) (pr-str (:error opened)))
        (is (:ok resumed) (pr-str (:error resumed)))
        (is (= :guided-solve (get-in resumed [:state :current-phase])))
        (is (:ok continued) (pr-str (:error continued)))
        (is (= :dispatch-solver (:tool (last (remove #(= :problem-save (:tool %))
                                                     (:log continued)))))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest conductor-requires-and-records-typed-guidance
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            untyped (conductor/guide-solver!
                     opened {:mission "M-test"} "untyped guidance")
            typed (conductor/guide-solver!
                   opened :suggest {:mission "M-test"} "typed guidance")]
        (is (= :guidance-type-absent
               (get-in untyped [:error :error/code])))
        (is (:ok typed) (pr-str (:error typed)))
        (is (= :suggest
               (->> (get-in typed [:state :steps])
                    (filter #(= :guide-solver (:tool %)))
                    last :result :ground-control/type))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest conductor-dispatches-the-registered-scribe-only-at-promote
  (let [{:keys [config paths]} (fixture)
        agent-id "scribe-dispatch-guide"
        session-id "scribe-dispatch-session"]
    (try
      (let [opened (conductor/open-frame! config)
            out-of-phase (conductor/dispatch-scribe!
                          opened {:mission "M-test"} "mine this cycle")
            promoted (-> opened
                         (assoc-in [:state :current-phase] :promote)
                         (update-in [:state :steps] conj
                                    {:tool :dispatch-solver
                                     :result {:job-id "solver-job"}}
                                    {:tool :dispatch-student-fresh
                                     :result {:job-id "student-job"}}))
            dispatched (conductor/dispatch-scribe!
                        promoted {:mission "M-test"} "mine this cycle")]
        (is (false? (:ok out-of-phase)))
        (is (:ok dispatched) (pr-str (:error dispatched)))
        (is (= :dispatch-scribe
               (->> (get-in dispatched [:state :steps])
                    (remove #(= :problem-save (:tool %))) last :tool)))
        (is (= "scribe-test"
               (->> (get-in dispatched [:state :steps])
                    (filter #(= :dispatch-scribe (:tool %))) last
                    :result :ground-control/recipient)))
        (let [sent-opts (->> (get-in dispatched [:state :steps])
                             (filter #(= :dispatch-scribe (:tool %))) last
                             :result :sent-opts)]
          (is (= "t94J02" (:problem-id sent-opts)))
          (is (= (:cycle-id promoted) (:cycle-id sent-opts)))
          (is (= ["solver-job"] (:solver-job-ids sent-opts)))
          (is (= ["student-job"] (:student-job-ids sent-opts)))
          (is (= "/home/joe/code/futon3c/holes/labs/M-apm-demonstration/role-cards/scribe.md"
                 (:scribe-card-path sent-opts))))
        (agency/register-agent!
         {:agent-id agent-id :type :claude
          :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})})
        (agency/update-agent! agent-id :agent/session-id session-id)
        (is (:ok (binding/install! agent-id session-id promoted)))
        (let [{:keys [cycle-id version]} (binding/status agent-id session-id)
              routed
              (conductor-surface/execute-action!
               agent-id session-id
               {:action-id "scribe-1" :cycle-id cycle-id :version version
                :operation :dispatch-scribe
                :args [{:mission "M-test"} "mine through surface"]})]
          (is (:ok routed) (pr-str routed))))
      (finally
        (binding/reset-bindings!)
        (agency/unregister-agent! agent-id)
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest open-frame-refuses-invalid-mode-and-threads-conductor
  (let [{:keys [config]} (fixture)]
    (let [bad (conductor/open-frame! (assoc config :mode nil))]
      (is (false? (:ok bad)))
      (is (re-find #"store-mode" (or (:error/message bad) (str bad)))))
    (let [h (conductor/open-frame! (assoc config
                                          :mode :store-mode
                                          :deposit-state :with-deposit))]
      (is (not (false? (:ok h))))
      (is (= :store-mode (get-in h [:state :cycle/mode])))
      (is (= :with-deposit (get-in h [:state :cycle/deposit-state]))))))

(deftest conductor-surface-authenticates-promotion-reviewer
  (let [agent-id "claude-review-actor"
        session-id "review-actor-session"
        captured (atom nil)
        promotion {:memory-id "e-memory"
                   :pattern-id "p4ng/pattern"
                   :reviewer agent-id}
        action {:action-id "review-action"
                :cycle-id "cycle-review"
                :version 1
                :operation :adjudicate
                :args [{:outcome :closed :promotion-result [promotion]}]}]
    (agency/register-agent!
     {:agent-id agent-id :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
      :session-id session-id})
    (with-redefs [binding/execute!
                  (fn [_ _ routed _]
                    (reset! captured routed)
                    {:ok true})]
      (is (:ok (conductor-surface/execute-action!
                agent-id session-id action)))
      (is (= agent-id
             (get-in @captured [:args 0 :promotion-result 0
                                :acting-identity])))
      (let [mismatched (assoc-in action
                                 [:args 0 :promotion-result 0 :reviewer]
                                 "some-other-reviewer")
            result (conductor-surface/execute-action!
                    agent-id session-id mismatched)]
        (is (false? (:ok result)))
        (is (= :reviewer-not-actor (:error/code result)))
        (is (= :reviewer-not-actor
               (get-in result [:finding :failure])))))))

(deftest conductor-action-route-owns-one-live-handle
  (let [{:keys [config paths]} (fixture)
        agent-id "claude-7"
        session-id "conductor-surface-session"
        handler (http/make-handler {})
        request!
        (fn [payload]
          (let [response
                (handler
                 {:request-method :post :uri "/api/alpha/conductor/action"
                  :body (json/generate-string
                         (merge {:agent-id agent-id :session-id session-id}
                                payload))})]
            (cond-> (assoc (json/parse-string (:body response) true)
                           :http/status (:status response))
              (string? (:error/code (json/parse-string (:body response) true)))
              (update :error/code keyword))))
        status!
        (fn [agent session]
          (let [response
                (handler
                 {:request-method :get :uri "/api/alpha/conductor/status"
                  :query-string (str "agent-id=" agent "&session-id=" session)})]
            (json/parse-string (:body response) true)))
        action!
        (fn [id operation args]
          (let [{:keys [cycle-id version]} (binding/status agent-id session-id)]
            (request! {:action-id id :operation (name operation) :args args
                       :cycle-id cycle-id :version version})))]
    (binding/reset-bindings!)
    (agency/register-agent!
     {:agent-id agent-id :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
      :session-id session-id})
    (agency/register-agent!
     {:agent-id "claude-unbound" :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id "no-session"})
      :session-id "no-session"})
    (try
      (let [opened (conductor/open-frame!
                    (assoc config :conductor
                           {:agent agent-id :session session-id
                            :surface "problem-conductor"}))
            before (count (get-in opened [:state :steps]))
            out-of-phase (action! "a-wrong" :dispatch-student
                                  [{:mission "M-test"} "student"])
            after-refusal (count (get-in @(:handle (binding/lookup agent-id session-id))
                                         [:state :steps]))
            dispatched (action! "a-solver" :dispatch-solver
                                [{:mission "M-test"} "solver"])
            after-dispatch @(:handle (binding/lookup agent-id session-id))
            replay (let [{:keys [cycle-id]} (binding/status agent-id session-id)]
                     (request! {:action-id "a-solver" :operation "dispatch-solver"
                                :args [{:mission "M-test"} "solver"]
                                :cycle-id cycle-id
                                :version (binding/handle-version after-dispatch)}))]
        (is (:ok opened) (pr-str (:error opened)))
        (is (= :phase-tool-not-allowed (:error/code out-of-phase)))
        (is (= before after-refusal) "a refused phase action records no step")
        (is (:ok dispatched))
        (is (= 1 (count (filter #(= :dispatch-solver (:tool %))
                                (get-in after-dispatch [:state :steps]))))
            "the routed action creates exactly one dispatch step")
        (is (= :conductor-action-duplicate (:error/code replay)))
        (is (= 1 (count (filter #(= :dispatch-solver (:tool %))
                                (get-in @(:handle (binding/lookup agent-id session-id))
                                        [:state :steps]))))
            "a replay cannot create a second step")
        (is (= :conductor-session-unbound
               (:error/code
                (request! {:agent-id "claude-unbound"
                           :session-id "no-session"
                           :action-id "a-unbound" :operation "close"
                           :args [] :cycle-id "none" :version 0}))))
        (is (= false (:bound? (status! "nobody" "no-session")))
            "read-only status is available without a binding")
        (let [{:keys [cycle-id version]} (binding/status agent-id session-id)]
          (is (= :conductor-operation-unknown
                 (:error/code
                  (request! {:action-id "a-unknown" :operation "eval"
                             :args [] :cycle-id cycle-id :version version}))))
          (is (= :conductor-cycle-stale
                 (:error/code
                  (request! {:action-id "a-stale-cycle" :operation "deposit"
                             :args [{}] :cycle-id "old-cycle"
                             :version version}))))
          (is (= :conductor-version-stale
                 (:error/code
                  (request! {:action-id "a-stale-version" :operation "deposit"
                             :args [{}] :cycle-id cycle-id
                             :version (dec version)})))))

        (is (:ok (action! "a-attempt" :record-solver-attempt
                          [(solver-attempt) {}])))
        (is (:ok (action! "a-deposit" :deposit
                          [{:name "deposit" :kind :feedback :hook "test"
                            :body {:lesson "surface"}
                            :subjects [{:ref/type :problem :ref/id "t94J02"}]}])))
        (is (:ok (action! "a-student" :dispatch-student
                          [{:mission "M-test"} "student"])))
        (is (:ok (action! "a-students" :record-students
                          [[(student-attempt)] []])))
        (let [adjudicated (action! "a-adjudicate" :adjudicate
                                   [{:outcome :tier-a :residual-sorries 1
                                     :axiom-clean? false :promotion-result []}])]
          (is (:ok adjudicated) (pr-str adjudicated)))
        (let [closed (action! "a-close" :close [])]
          (is (:ok closed) (pr-str closed)))
        (is (= false (:bound? (status! agent-id session-id))))
        (is (= :conductor-session-unbound
               (:error/code
                (request! {:action-id "a-after" :operation "close" :args []
                           :cycle-id (:cycle-id opened) :version 0})))
            "the sentinel removes the transport route"))
      (finally
        (binding/reset-bindings!)
        (agency/unregister-agent! agent-id)
        (agency/unregister-agent! "claude-unbound")
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest conductor-abandon-route-authenticates-and-releases-live-binding
  (let [agent-id "claude-abandon"
        session-id "abandon-session"
        cycle-id "cycle-abandon"
        handler (http/make-handler {})
        handle {:cycle-id cycle-id
                :state {:current-phase :guided-solve}
                :log []}
        post! (fn [version]
                (let [response
                      (handler {:request-method :post
                                :uri "/api/alpha/conductor/abandon"
                                :body (json/generate-string
                                       {:agent-id agent-id
                                        :session-id session-id
                                        :cycle-id cycle-id
                                        :version version})})]
                  (assoc (json/parse-string (:body response) true)
                         :http/status (:status response))))]
    (binding/reset-bindings!)
    (agency/register-agent!
     {:agent-id agent-id :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
      :session-id session-id})
    (try
      (is (:ok (binding/install! agent-id session-id handle)))
      (let [stale (post! 1)]
        (is (= 409 (:http/status stale)))
        (is (= "conductor-abandonment-stale" (:error/code stale)))
        (is (some? (binding/lookup agent-id session-id))))
      (let [released (post! 0)]
        (is (= 200 (:http/status released)))
        (is (:abandoned? released))
        (is (nil? (binding/lookup agent-id session-id))))
      (let [unbound (post! 0)]
        (is (= 409 (:http/status unbound)))
        (is (= "conductor-session-unbound" (:error/code unbound))))
      (finally
        (binding/reset-bindings!)
        (agency/reset-registry!)))))

(deftest conductor-takeover-loads-the-named-version-and-preserves-parked-binding
  (let [{:keys [config paths]} (fixture)
        old-agent "claude-old"
        old-session "surface-old"
        new-agent "claude-new"
        new-session "surface-new"
        handler (http/make-handler {})
        post! (fn [uri payload]
                (let [response (handler {:request-method :post :uri uri
                                         :body (json/generate-string payload)})
                      body (json/parse-string (:body response) true)]
                  (cond-> (assoc body :http/status (:status response))
                    (string? (:error/code body)) (update :error/code keyword))))]
    (binding/reset-bindings!)
    (doseq [[agent session] [[old-agent old-session] [new-agent new-session]]]
      (agency/register-agent!
       {:agent-id agent :type :claude :session-id session
        :invoke-fn (fn [_ _] {:result "unused" :session-id session})}))
    (try
      (let [opened (conductor/open-frame!
                    (assoc config :conductor
                           {:agent old-agent :session old-session
                            :surface "problem-conductor"}))
            {:keys [cycle-id version]} (binding/status old-agent old-session)
            before @(:handle (binding/lookup old-agent old-session))
            wrong (post! "/api/alpha/conductor/takeover"
                         {:agent-id new-agent :session-id new-session
                          :cycle-id cycle-id :version (dec version)})]
        (is (:ok opened) (pr-str (:error opened)))
        (is (= :conductor-version-stale (:error/code wrong)))
        (is (= version (:version (binding/status old-agent old-session)))
            "a refused takeover leaves the old authority intact")
        (is (= :conductor-binding-exists
               (:error/code
                (post! "/api/alpha/conductor/takeover"
                       {:agent-id old-agent :session-id old-session
                        :cycle-id cycle-id :version version})))
            "a live session cannot replace its binding through takeover")

        ;; Simulate the old conductor process disappearing. The server-owned
        ;; binding remains available for an explicit versioned transfer.
        (agency/unregister-agent! old-agent)

        (let [taken (post! "/api/alpha/conductor/takeover"
                           {:agent-id new-agent :session-id new-session
                            :cycle-id cycle-id :version version})
              after-takeover (binding/status new-agent new-session)
              wake-version (:version after-takeover)
              wake (post! "/api/alpha/conductor/resume"
                          {:agent-id new-agent :session-id new-session
                           :cycle-id cycle-id :version wake-version})
              stale-wake (post! "/api/alpha/conductor/resume"
                                {:agent-id new-agent :session-id new-session
                                 :cycle-id cycle-id :version version})]
          (is (:ok taken) (pr-str taken))
          (is (= false (:bound? (binding/status old-agent old-session))))
          (is (:bound? after-takeover))
          (is (> wake-version version)
              "loading the named save is checkpointed as the next store version")
          (is (= (get-in before [:state :current-phase])
                 (:phase after-takeover)))
          (is (:ok wake))
          (is (= wake-version (:version (binding/status new-agent new-session)))
              "waking a prose continuation does not mutate the handle")
          (is (= :conductor-version-stale (:error/code stale-wake))
              "stale parked metadata is refused before an action")

          ;; Reconnect is transport state only: the server-owned cycle survives.
          (agency/unregister-agent! new-agent)
          (agency/register-agent!
           {:agent-id new-agent :type :claude :session-id new-session
            :invoke-fn (fn [_ _] {:result "unused" :session-id new-session})})
          (is (:bound? (binding/status new-agent new-session)))

          (let [routed (post! "/api/alpha/conductor/action"
                              {:agent-id new-agent :session-id new-session
                               :action-id "after-takeover"
                               :operation "dispatch-solver"
                               :args [{:mission "M-test"} "continued"]
                               :cycle-id cycle-id :version wake-version})
                authoritative @(:handle (binding/lookup new-agent new-session))]
            (is (:ok routed) (pr-str routed))
            (is (= 1 (count (filter #(= :dispatch-solver (:tool %))
                                    (get-in authoritative [:state :steps]))))
                "the taken-over cycle continues only through the typed route"))))
      (finally
        (binding/reset-bindings!)
        (agency/unregister-agent! old-agent)
        (agency/unregister-agent! new-agent)
        (doseq [path paths] (Files/deleteIfExists path))))))
