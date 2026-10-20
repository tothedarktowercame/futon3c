(ns futon3c.wm.runner-service-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as service]
            [futon3c.wm.meta-self-heal-adapters :as self-heal-adapters]
            [futon3c.wm.ordinary-click-budget :as budget]
            [futon3c.wm.machinery-execution-cohort :as cohort])
  (:import [java.time Instant]))

(def scratch-agent-id "war-machine")

(defn- await-active-click! []
  (when-let [click-id (:click-id (service/status))]
    ;; The bounded test service owns the suite-level deadline. Locally, drain
    ;; the exact worker before mutating shared state; a short local deadline
    ;; was the mechanism that allowed a late worker into the next fixture.
    (let [completion (service/await-click! click-id)]
      (is (= :completed (:status completion))
          (str "runner fixture did not join actual worker completion: "
               (pr-str completion))))))

(defn- reset-service!
  [f]
  ;; A projected :running? false is not a thread join. The old fixture reset
  ;; !status immediately and let the prior worker's report-idle! mutate the
  ;; next test's freshly reset registry. Join the real worker on both sides.
  (await-active-click!)
  (reg/reset-registry!)
  (reset! service/!status service/initial-status)
  (reg/register-agent!
   {:agent-id {:id/value scratch-agent-id :id/type :apparatus}
    :type :wm
    :invoke-fn nil
    :capabilities []
    :metadata {:apparatus? true}})
  (let [binding-dir (.getPath
                     (.toFile
                      (java.nio.file.Files/createTempDirectory
                       "wm-click-bindings-test-"
                       (make-array java.nio.file.attribute.FileAttribute 0))))]
    (binding [service/*click-run-binding-dir* binding-dir
              service/*click-acceptance-dir* (str binding-dir "/acceptances")
              budget/*ledger-path* (str binding-dir "/ordinary.jsonl")
              ;; Never launch the real route-check script from a unit test.
              service/*launch-route-check!* (fn [_ _] {:id "bg-fixture"})]
      (try
        (f)
        (finally
          (await-active-click!))))))

(use-fixtures :each reset-service!
  (fn [f] (with-redefs [cohort/apply-binding identity] (f))))

(defn- handler []
  (http/make-handler
   {:registry (fix/mock-registry)
    :patterns (fix/mock-patterns)}))

(defn- response-body [response]
  (json/parse-string (:body response) true))

(declare resolver)

(deftest meta-producer-prompts-declare-operation-specific-edn-contracts
  (let [prompts (atom [])
        request {:injury-id "injury-1"
                 :source-record-pin {:path "source.edn" :sha256 (apply str (repeat 64 "a"))}
                 :diagnosis-evidence-pin {:path "diagnosis.edn" :sha256 (apply str (repeat 64 "b"))}
                 :proposal-evidence-pin {:path "proposal.edn" :sha256 (apply str (repeat 64 "c"))}
                 :acceptance-test {:id "case" :command ["not-run"] :expected :observed}}
        invoke (fn [_ prompt _]
                 (swap! prompts conj prompt)
                 {:ok true :result "{:status :failed :reason :fixture}"})]
    (with-redefs [reg/get-agent (constantly {:agent/status :idle
                                              :agent/invoke-ready? true
                                              :agent/session-id "session-1"})
                  reg/invoke-agent! invoke]
      (#'self-heal-adapters/producer-invoke! "wm-author" :diagnosis request)
      (#'self-heal-adapters/producer-invoke! "wm-author" :acceptance-proposal request)
      (#'self-heal-adapters/producer-invoke! "wm-reviewer" :acceptance-review request))
    (let [[diagnosis proposal review] @prompts]
      (is (str/includes? diagnosis ":status :succeeded"))
      (is (str/includes? diagnosis ":diagnosis"))
      (is (str/includes? proposal ":status :proposed"))
      (is (str/includes? proposal ":acceptance-test"))
      (is (str/includes? proposal ":command"))
      (is (str/includes? proposal ":expected"))
      (is (str/includes? review ":status :accepted"))
      (doseq [key [":injury-id" ":source-record-pin" ":diagnosis-evidence-pin"
                   ":proposal-evidence-pin" ":acceptance-test"]]
        (is (str/includes? review key)))
      (doseq [prompt @prompts]
        (is (str/includes? prompt "Do not select ordinary work"))
        ;; F15 (claude-5, fd12530c8, 2026-10-19) reworded the prohibition to
        ;; "run tests, builds or code"; this assertion was left on the old text.
        (is (str/includes? prompt "run tests, builds or code"))
        (is (str/includes? prompt "claim a repair"))))))

(deftest meta-producer-invokes-a-restored-seat-through-its-registered-handler
  (let [calls (atom [])]
    (with-redefs [reg/get-agent
                  (constantly {:agent/status :restored
                               :agent/invoke-fn (fn [& _])
                               :agent/session-id nil})
                  reg/invoke-agent!
                  (fn [seat prompt opts]
                    (swap! calls conj [seat prompt opts])
                    {:ok true :result "{:status :failed :reason :fixture}"})]
      (is (= {:status :failed :reason :fixture}
             (#'self-heal-adapters/producer-invoke!
              "wm-author" :diagnosis {:injury-id "injury-1"}))))
    (is (= 1 (count @calls)))
    (is (= "wm-author" (ffirst @calls)))))

(deftest durable-idempotent-acceptance-is-per-click-not-last-result
  (let [release (promise)
        entered (promise)
        charges (atom 0)
        run! (fn [_] (deliver entered true) @release
               {:attempt-id "slow" :outcome :grounded-change})
        opts {:wm-agent-id scratch-agent-id
              :idempotency-key "request-1"
              :acceptance-payload {:cast ["wm-author" "wm-reviewer"]}
              :ordinary-click/issue! (fn [_ _] (swap! charges inc))}]
    (binding [service/*resolve-var* (resolver run! (constantly {:selected-policy-id "unused"}))]
      (let [accepted (service/click! opts)
            _ (is (= true (deref entered 1000 false)))
            retry (service/click! opts)
            click-id (:click-id accepted)]
        (is (= :queued (:status accepted)))
        (is (= click-id (:click-id retry)))
        (is (= 1 @charges))
        (is (contains? #{:queued :running} (:status (service/click-status click-id))))
        ;; A stale singleton result for another click cannot satisfy this query.
        (swap! service/!status assoc :last-result {:click-id "older" :outcome :grounded-change})
        (is (= click-id (:click-id (service/click-status click-id))))
        (let [conflict (try (service/click! (assoc opts :acceptance-payload {:different true})) nil
                            (catch clojure.lang.ExceptionInfo t t))]
          (is (= :wm-click-idempotency-conflict (:error (ex-data conflict))))
          (is (= 1 @charges)))
        (is (= :timed-out (:status (service/await-click! click-id 1))))
        (deliver release true)
        (is (= :completed (:status (service/await-click! click-id))))
        (is (= :completed (:status (service/click-status click-id))))))))

(deftest route-check-launch-sees-the-completed-click-state-and-never-rewrites-it
  (let [seen (atom nil)
        run! (fn [_] {:attempt-id "route-after-close" :outcome :grounded-change})
        opts {:wm-agent-id scratch-agent-id
              :idempotency-key "route-after-close"
              :acceptance-payload {:cast ["wm-author" "wm-reviewer"]}
              :ordinary-click/issue! (fn [& _] nil)}]
    (binding [service/*resolve-var* (resolver run! (constantly {:selected-policy-id "unused"}))
              service/*launch-route-check!*
              (fn [_ click-id]
                (let [bytes (slurp (#'service/acceptance-file click-id))]
                  (reset! seen {:bytes bytes :state (edn/read-string bytes)})
                  {:id "bg-route" :pid 42}) )]
      (let [click-id (:click-id (service/click! opts))]
        (is (= :completed (:status (service/await-click! click-id))))
        (let [after (slurp (#'service/acceptance-file click-id))]
          (is (= :completed (get-in @seen [:state :status])))
          (is (= after (:bytes @seen)) "the checker saw the one completed write")
          (is (not (contains? (get-in @seen [:state :result]) :route-check)))
          (is (= {:status :launched :job {:id "bg-route" :pid 42}}
                 (get-in (service/status) [:last-result :route-check]))))))))

(deftest http-lost-response-retry-and-per-click-query
  (let [release (promise)
        run! (fn [_] @release {:attempt-id "http-slow" :outcome :grounded-change})
        h (handler)
        request {:request-method :post :uri "/api/alpha/wm/click"
                 :body (json/generate-string
                        {:author "zai-2" :reviewer "codex-2" :repair-reviewer "codex-1"
                         :idempotency-key "lost-http-response"})}]
    (binding [service/*resolve-var* (resolver run! (constantly {:selected-policy-id "unused"}))]
      (let [first (response-body (h request))
            retry (response-body (h request))
            click-id (:click-id first)
            queried (response-body
                     (h {:request-method :get :uri "/api/alpha/wm/click"
                         :query-string (str "click-id=" click-id)}))]
        (is (= click-id (:click-id retry)))
        (is (= click-id (:click-id queried)))
        (is (contains? #{"queued" "running"} (:status queried)))
        (deliver release true)
        (is (= :completed (:status (service/await-click! click-id))))))))

(defn- wait-until
  [predicate timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (predicate) true
        (< (System/currentTimeMillis) deadline)
        (do (Thread/sleep 5) (recur))
        :else false))))

(deftest unsupported-ordinary-trigger-is-rejected-before-any-lifecycle-side-effect
  (let [issued (atom 0)
        before (service/status)]
    (binding [service/*click-run-binding-dir*
              (.getPath (.toFile (java.nio.file.Files/createTempDirectory
                                  "unsupported-trigger-"
                                  (make-array java.nio.file.attribute.FileAttribute 0))))]
      (let [failure (try
                      (service/click! {:trigger :custom-operator-trigger
                                       :ordinary-click/issue! #(swap! issued inc)})
                      nil
                      (catch clojure.lang.ExceptionInfo throwable throwable))]
        (is (= :wm-click-trigger-unsupported (:error (ex-data failure))))
        (is (= :custom-operator-trigger (:trigger (ex-data failure))))
        (is (= #{:duree-click-on-demand}
               (:accepted-triggers (ex-data failure))))
        (is (zero? @issued) "ration callback was not invoked")
        (is (= (dissoc before :serving-runner-code)
               (dissoc (service/status) :serving-runner-code))
            "no click id or running status was published")
        (is (empty? (seq (.listFiles (io/file service/*click-run-binding-dir*))))
            "no click/run binding was created")))))

(deftest supported-ordinary-trigger-passes-the-authoritative-admission
  (is (= :duree-click-on-demand
         (:trigger (service/admit-ordinary-click-trigger! {}))))
  (is (= :duree-click-on-demand
         (:trigger (service/admit-ordinary-click-trigger!
                    {:trigger :duree-click-on-demand})))))

(deftest caller-run-id-admission-precedes-click-lifecycle
  (let [valid "2099-12-31-1084e87d-7154-46c8-a70b-681589061d3d"
        before (service/status)
        issued (atom 0)
        malformed (try
                    (service/click! {:run-id "2026-10-02-"
                                     :ordinary-click/issue! #(swap! issued inc)})
                    nil
                    (catch clojure.lang.ExceptionInfo throwable throwable))]
    (is (= :wm-click-run-id-malformed (:error (ex-data malformed))))
    (is (= 400 (:status (ex-data malformed))))
    (is (zero? @issued))
    (is (= (dissoc before :serving-runner-code)
           (dissoc (service/status) :serving-runner-code)))
    (is (= valid (:run-id (service/admit-run-id! {:run-id valid}))))
    (is (= {} (service/admit-run-id! {})))))

(deftest existing-run-record-refuses-caller-id-before-click
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "wm-run-id-collision-"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        run-id "2026-10-02-1084e87d-7154-46c8-a70b-681589061d3d"]
    (try
      (spit (io/file root (str "tick-run-record-" run-id ".edn")) "{}")
      (binding [service/*run-record-dir* (.getPath root)]
        (let [failure (try (service/admit-run-id! {:run-id run-id}) nil
                           (catch clojure.lang.ExceptionInfo throwable throwable))]
          (is (= :wm-click-run-id-collision (:error (ex-data failure))))
          (is (= 409 (:status (ex-data failure))))))
      (finally (doseq [file (reverse (file-seq root))]
                 (io/delete-file file true))))))

(deftest http-rejects-truncated-run-id-before-calling-runner
  (let [calls (atom 0)
        response (with-redefs [service/click! (fn [_] (swap! calls inc))]
                   ((handler) {:request-method :post
                               :uri "/api/alpha/wm/click"
                               :body (json/generate-string
                                      {:run-id "2026-10-02-"})}))]
    (is (= 400 (:status response)))
    (is (= "wm-click-run-id-malformed" (:error (response-body response))))
    (is (zero? @calls))))

(deftest http-preserves-valid-and-omitted-run-id-contract
  (let [seen (atom [])
        valid "2099-12-31-3084e87d-7154-46c8-a70b-681589061d3d"
        invoke (fn [payload]
                 ((handler) {:request-method :post :uri "/api/alpha/wm/click"
                             :body (json/generate-string payload)}))]
    (with-redefs [service/prepare-ordinary-click-opts identity
                  service/cast-preflight-refusal (constantly nil)
                  service/click! (fn [opts]
                                   (swap! seen conj opts)
                                   {:click-id "not-launched" :started-at "fixture"})]
      (is (= 200 (:status (invoke {:run-id valid}))))
      (is (= 200 (:status (invoke {}))))
      (is (= valid (:run-id (first @seen))))
      (is (not (contains? (second @seen) :run-id))
          "omission reaches the runner as omission, so it mints canonically"))))

(defn- resolver
  [run! select]
  (fn [sym]
    (case sym
      futon2.aif.full-loop-runner/config identity
      futon2.aif.full-loop-runner/agent-roster
      (fn [_]
        {:zai-2 {:status "idle" :invoke-ready? true}
         :codex-2 {:status "idle" :invoke-ready? true}
         :codex-1 {:status "idle" :invoke-ready? true}})
      futon2.aif.full-loop-runtime/run-opportunity!
      (fn [opts]
        ;; Model the real runner's :admission phase: a successful readiness
        ;; decision invokes this seam before later phases or the fake body.
        (when-let [admit! (:readiness-admitted-fn opts)] (admit!))
        (run! opts))
      futon3c.peripheral.live-wm-selection/validated-selection select
      futon3c.wm.scheduler/ensure-war-machine-agent! (fn [] nil)
      nil)))

(deftest single-flight-routes-and-registry-status
  (let [release (promise)
        phase-seen (promise)
        selection-request (atom nil)
        runner-opts-seen (atom nil)
        runner-thread-seen (atom nil)
        runner-daemon-seen (atom nil)
        run!
        (fn [opts]
          (reset! runner-opts-seen opts)
          (reset! runner-thread-seen (.getName (Thread/currentThread)))
          (reset! runner-daemon-seen (.isDaemon (Thread/currentThread)))
          (let [selection ((:strategic-selection-invoke-fn opts)
                           {:scheduler-habit-ranking ["M-test"]
                            :trace-id "trace-test"})]
            (is (= {:ok true :selection {:selected-policy-id "pi-test"}}
                   selection)))
          ((:phase-log-fn opts)
           {:phase :author-wait
            :transition :start
            :attempt-id "attempt-test"})
          (deliver phase-seen true)
          @release
          ((:phase-log-fn opts)
           {:phase :opportunity
            :transition :end
            :attempt-id "attempt-test"
            :outcome :grounded-change})
          {:attempt-id "attempt-test" :outcome :grounded-change
           :status :refused
           :reason :meta-self-heal/acceptance-transaction-unadmitted
           :route :dedicated-meta-self-heal
           :ordinary-selection-called? false})
        select (fn [request]
                 (reset! selection-request request)
                 {:selected-policy-id "pi-test"})
        h (handler)]
    (binding [service/*resolve-var* (resolver run! select)]
      (let [first-response
            (h {:request-method :post
                :uri "/api/alpha/wm/click"
                :body (json/generate-string
                       {:author "zai-2"
                        :reviewer "codex-2"
                        :repair-reviewer "codex-1"
                        :run-id "2099-12-31-2084e87d-7154-46c8-a70b-681589061d3d"})})
            first-body (response-body first-response)
            _ (is (= true (deref phase-seen 1000 false)))
            second-response
            (h {:request-method :post
                :uri "/api/alpha/wm/click"
                :body "{}"})
            second-body (response-body second-response)
            direct-second (service/click! {:author "zai-2"
                                           :reviewer "codex-2"
                                           :repair-reviewer "codex-1"})
            get-response
            (h {:request-method :get :uri "/api/alpha/wm/click"})
            get-body (response-body get-response)
            invoking-agent (reg/get-agent scratch-agent-id)
            active-at (:agent/last-active invoking-agent)]
        (is (= 200 (:status first-response)))
        (is (string? (:click-id first-body)))
        (is (string? (:started-at first-body)))
        (is (= 409 (:status second-response)))
        ;; The HTTP route performs live cast preflight before entering click!,
        ;; so the already-invoking cast is refused there. The service remains
        ;; the single-flight authority for callers that already hold opts.
        (is (= "wm-click-cast-unavailable" (:error second-body)))
        (is (= :already-running (:rejected direct-second)))
        (is (= (:click-id first-body) (:click-id direct-second)))
        (is (= 200 (:status get-response)))
        (is (true? (:running? get-body)))
        (is (= "author-wait" (:phase get-body)))
        (is (= "attempt-test" (:attempt-id get-body)))
        (is (= :invoking (:agent/status invoking-agent)))
        (is (= "author-wait attempt-test"
               (:agent/invoke-activity invoking-agent)))
        (is (instance? Instant active-at))
        (is (= {:scheduler-habit-ranking ["M-test"]
                :trace-id "trace-test"}
               @selection-request))
        (is (= "wm-runner-click" @runner-thread-seen))
        (is (true? @runner-daemon-seen))
        (is (= "zai-2" (:author @runner-opts-seen)))
        (is (= "codex-2" (:reviewer @runner-opts-seen)))
        (is (= "codex-1" (:repair-reviewer @runner-opts-seen)))
        (is (= "2099-12-31-2084e87d-7154-46c8-a70b-681589061d3d"
               (:run-id @runner-opts-seen)))
        (deliver release true)
        (is (wait-until #(false? (:running? (service/status))) 5000))
        (let [closed (service/status)
              idle-agent (reg/get-agent scratch-agent-id)]
          (is (= :idle (:agent/status idle-agent)))
          (is (nil? (:agent/invoke-activity idle-agent)))
          (is (not (.isBefore ^Instant (:agent/last-active idle-agent)
                              ^Instant active-at)))
          (is (= {:attempt-id "attempt-test"
                  :outcome :grounded-change
                  :runtime-disposition
                  {:status :refused
                   :reason :meta-self-heal/acceptance-transaction-unadmitted
                   :route :dedicated-meta-self-heal
                   :ordinary-selection-called? false}
                  :binding-status :absent
                  :run-id-observation
                  {:status :absent :reason :runner-did-not-return-run-id}
                  :run-record-status :absent}
                 (select-keys (:last-result closed)
                              [:attempt-id :outcome :runtime-disposition :binding-status
                               :run-id-observation :run-record-status])))
          ;; F7: the refused click's disposition is retained in its durable
          ;; binding, not only in the serving last-result.
          (let [binding-file (io/file service/*click-run-binding-dir*
                                      (str "click-run-binding-" (:click-id first-body) ".edn"))]
            (is (.exists binding-file))
            (is (= {:status :refused
                    :reason :meta-self-heal/acceptance-transaction-unadmitted
                    :route :dedicated-meta-self-heal
                    :ordinary-selection-called? false}
                   (:runtime-disposition (edn/read-string (slurp binding-file)))))))))))

(deftest service-source-has-no-process-spawn-surface
  (let [source (slurp
                (io/file "src/futon3c/wm/runner_service.clj"))]
    (testing "the in-process service does not import or invoke process APIs"
      (is (not (str/includes? source "clojure.java.shell")))
      (is (not (str/includes? source "ProcessBuilder")))
      (is (not (re-find #"\(\s*sh/" source))))))

(deftest runner-resolution-failure-releases-single-flight
  (binding [service/*resolve-var*
            (fn [sym]
              (case sym
                futon2.aif.full-loop-runner/config identity
                (throw (ex-info "runner unavailable" {:symbol sym}))))]
    (let [accepted (service/click! {:wm-agent-id scratch-agent-id})
          click-id (:click-id accepted)]
      (is (= :queued (:status accepted)))
      (is (wait-until #(false? (:running? (service/status))) 5000))
      (is (= :completed (:status (service/await-click! click-id))))
      (is (= :service-failed
             (get-in (service/status) [:last-result :outcome])))
      (is (= :idle
             (:agent/status (reg/get-agent scratch-agent-id)))))))

(deftest completion-signal-survives-a-status-projection-reset
  (let [release (promise)
        entered (promise)
        run! (fn [_]
               (deliver entered true)
               @release
               {:attempt-id "attempt-race-control" :outcome :grounded-change})]
    (binding [service/*resolve-var*
              (resolver run! (fn [_] {:selected-policy-id "unused"}))]
      (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= true (deref entered 1000 false)))
        ;; This is the pre-fix ordering: the next fixture erased the status
        ;; projection while the prior worker was still alive.
        (reset! service/!status service/initial-status)
        (is (= :timed-out (:status (service/await-click! click-id 1))))
        (deliver release true)
        (is (= {:status :completed :click-id click-id}
               (service/await-click! click-id)))))))

(deftest lifecycle-state-precedes-slow-registry-publication
  (let [phase-entered (promise)
        release-phase (promise)
        close-entered (promise)
        release-close (promise)
        run! (fn [opts]
               ((:phase-log-fn opts)
                {:phase :author-wait
                 :attempt-id "attempt-close-order"})
               {:attempt-id "attempt-close-order"
                :outcome :grounded-change})]
    (with-redefs [reg/update-agent!
                  (fn [& _]
                    (deliver phase-entered true)
                    @release-phase)
                  reg/mark-agent-idle!
                  (fn [& _]
                    (deliver close-entered true)
                    @release-close)]
      (binding [service/*resolve-var*
                (resolver run! (fn [_] {:selected-policy-id "unused"}))]
        (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
          (is (= true (deref phase-entered 1000 false)))
          (is (= :author-wait (:phase (service/status))))
          (is (= {:status :pending :stage :phase}
                 (:registry-publication (service/status))))
          (deliver release-phase true)
          (is (= true (deref close-entered 1000 false)))
          (is (false? (:running? (service/status))))
          (is (= {:attempt-id "attempt-close-order"
                  :outcome :grounded-change
                  :binding-status :absent
                  :run-id-observation
                  {:status :absent :reason :runner-did-not-return-run-id}
                  :run-record-status :absent}
                 (select-keys (:last-result (service/status))
                              [:attempt-id :outcome :binding-status
                               :run-id-observation :run-record-status])))
          (is (= {:status :pending :stage :close}
                 (:registry-publication (service/status))))
          ;; Publication remains synchronous and observable; only the
          ;; authoritative service close no longer waits behind it.
          (is (= :timed-out (:status (service/await-click! click-id 1))))
          (deliver release-close true)
          (is (= :completed (:status (service/await-click! click-id))))
          (is (= :published
                 (get-in (service/status) [:registry-publication :status]))))))))

(deftest returned-run-id-without-record-is-observed-but-unavailable
  (let [run-id "run-exact-binding"
        run! (fn [_] {:attempt-id "attempt-exact-binding"
                      :outcome :grounded-change
                      :run/id run-id})]
    (binding [service/*resolve-var*
              (resolver run! (fn [_] {:selected-policy-id "unused"}))]
      (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :completed (:status (service/await-click! click-id))))
        (let [last-result (:last-result (service/status))
              record (edn/read-string (slurp (:run-binding last-result)))]
          (is (= {:click-id click-id
                  :binding-status :unavailable
                  :run-id-observation
                  {:status :present :source :runner-return :value run-id}}
                 (select-keys last-result
                              [:click-id :binding-status :run-id-observation])))
          (is (= {:click/id click-id
                  :binding-status :unavailable
                  :run-id-observation
                  {:status :present :source :runner-return :value run-id}}
                 (select-keys record
                              [:click/id :binding-status :run-id-observation])))
          (is (not (contains? last-result :run/id)))
          (is (not (contains? record :run-id-status))))))))

(deftest pre-rename-binding-failure-is-absent-and-service-failed
  (let [run! (fn [_] {:attempt-id "attempt-binding-not-committed"
                      :outcome :grounded-change
                      :run/id "run-binding-not-committed"})]
    (binding [service/*resolve-var*
              (resolver run! (fn [_] {:selected-policy-id "unused"}))
              service/*click-run-binding-persist-stage-hook*
              (fn [stage _]
                (when (= :temp-forced stage)
                  (throw (ex-info "pre-rename failure" {}))))]
      (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :completed (:status (service/await-click! click-id))))
        (is (= :service-failed
               (get-in (service/status) [:last-result :outcome])))
        (is (false?
             (.exists (io/file service/*click-run-binding-dir*
                               (str "click-run-binding-" click-id ".edn")))))))))

(deftest post-rename-binding-warning-closes-consistently
  (let [run-id "run-binding-unconfirmed"
        run! (fn [_] {:attempt-id "attempt-binding-unconfirmed"
                      :outcome :grounded-change
                      :run/id run-id})]
    (binding [service/*resolve-var*
              (resolver run! (fn [_] {:selected-policy-id "unused"}))
              service/*click-run-binding-persist-stage-hook*
              (fn [stage _]
                (when (= :renamed stage)
                  (throw (ex-info "directory force failure" {}))))]
      (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :completed (:status (service/await-click! click-id))))
        (let [last-result (:last-result (service/status))
              record (edn/read-string (slurp (:run-binding last-result)))]
          (is (false? (:running? (service/status))))
          (is (= :grounded-change (:outcome last-result) (:outcome record)))
          (is (= run-id
                 (get-in last-result [:run-id-observation :value])
                 (get-in record [:run-id-observation :value])))
          (is (= :unconfirmed (:binding-durability last-result)))
          (is (= "directory force failure"
                 (get-in last-result [:binding-durability-warning :cause]))))))))

(deftest mismatched-run-record-is-typed-and-rejected
  (let [record-path (str (io/file service/*click-run-binding-dir* "wrong-run.edn"))
        run! (fn [opts]
               (io/make-parents record-path)
               (spit record-path
                     (pr-str {:run/id "different-run"
                              :click/id (:click-id opts)
                              :startedAt "2026-08-31T00:00:00Z"
                              :route [{:fromNode "R1" :toNode "R2"
                                       :via "control" :at_ "2026-08-31T00:00:01Z"}]}))
               {:attempt-id "attempt-record-mismatch"
                :outcome :grounded-change
                :run/id "expected-run"
                :run-record record-path})]
    (binding [service/*resolve-var*
              (resolver run! (fn [_] {:selected-policy-id "unused"}))]
      (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :completed (:status (service/await-click! click-id))))
        (is (= :identity-mismatch
               (get-in (service/status) [:last-result :run-record-status])))
        (is (= :identity-mismatch
               (get-in (service/status) [:last-result :binding-status])))
        (is (not (contains? (:last-result (service/status)) :run/id)))
        (is (= :run-record-identity-mismatch
               (get-in (service/status) [:last-result :run-record-absence])))))))

(deftest matching-run-record-is-authoritatively-verified
  (let [run-id "run-verified"
        run! (fn [opts]
               (let [record-path (str (io/file service/*click-run-binding-dir*
                                               "verified-run.edn"))]
                 (spit record-path
                       (pr-str {:run/id run-id :click/id (:click-id opts)}))
                 {:attempt-id "attempt-verified"
                  :outcome :grounded-change
                  :run/id run-id
                  :run-record record-path}))]
    (binding [service/*resolve-var*
              (resolver run! (fn [_] {:selected-policy-id "unused"}))]
      (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :completed (:status (service/await-click! click-id))))
        (is (= :verified
               (get-in (service/status) [:last-result :binding-status])))
        (is (= run-id
               (get-in (service/status)
                       [:last-result :run-id-observation :value])))))))

(deftest repeated-returned-id-is-authoritatively-duplicate
  (let [run-id "run-duplicate"
        run! (fn [_] {:attempt-id "attempt-duplicate"
                      :outcome :grounded-change
                      :run/id run-id})]
    (binding [service/*resolve-var*
              (resolver run! (fn [_] {:selected-policy-id "unused"}))]
      (let [{first-click :click-id} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :completed (:status (service/await-click! first-click))))
        (is (= :unavailable
               (get-in (service/status) [:last-result :binding-status])))
        (let [{second-click :click-id} (service/click! {:wm-agent-id scratch-agent-id})]
          (is (= :completed (:status (service/await-click! second-click))))
          (is (= :duplicate
                 (get-in (service/status) [:last-result :binding-status])))
          (is (= [first-click]
                 (get-in (service/status)
                         [:last-result :duplicate-of-clicks])))
          (is (= run-id
                 (get-in (service/status)
                         [:last-result :run-id-observation :value]))))))))

(deftest registry-publication-failure-does-not-rewrite-terminal-result
  (let [run! (fn [_]
               {:attempt-id "attempt-publication-failure"
                :outcome :grounded-change})]
    (with-redefs [reg/mark-agent-idle!
                  (fn [& _]
                    (throw (ex-info "registry unavailable" {:control true})))]
      (binding [service/*resolve-var*
                (resolver run! (fn [_] {:selected-policy-id "unused"}))]
        (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
          (is (= :completed (:status (service/await-click! click-id))))
          (is (= :grounded-change
                 (get-in (service/status) [:last-result :outcome])))
          (is (= {:status :failed
                  :stage :close
                  :error-class "clojure.lang.ExceptionInfo"
                  :cause "registry unavailable"}
                 (:registry-publication (service/status)))))))))

(deftest failed-click-state-precedes-failed-idle-publication
  (with-redefs [reg/mark-agent-idle!
                (fn [& _]
                  (throw (ex-info "registry unavailable" {:control true})))]
    (binding [service/*resolve-var*
              (resolver (fn [_] (throw (ex-info "runner failed" {})))
                        (fn [_] {:selected-policy-id "unused"}))]
      (let [{:keys [click-id]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :completed (:status (service/await-click! click-id))))
        (is (false? (:running? (service/status))))
        (is (= :service-failed
               (get-in (service/status) [:last-result :outcome])))
        (is (= :failure
               (get-in (service/status) [:registry-publication :stage])))
        (is (= :failed
               (get-in (service/status) [:registry-publication :status])))))))


(deftest failure-summary-retains-structured-projection-diagnostics
  (doseq [throwable [(ex-info "RUN4 terminal projection refused"
                             {:error :run4-terminal-projection-refused
                              :reason :malformed-checkpoint :checkpoint :adjudication})
                     (Exception. "plain failure")]]
    (binding [service/*resolve-var*
              (fn [sym]
                (case sym
                  futon2.aif.full-loop-runner/config identity
                  (throw throwable)))]
      (let [accepted (service/click! {:wm-agent-id scratch-agent-id})
            click-id (:click-id accepted)]
        (is (= :queued (:status accepted)))
        (is (= :completed (:status (service/await-click! click-id))))
        (let [summary (:last-result (service/status))]
          (is (= :service-failed (:outcome summary)))
          (is (= (.getMessage throwable) (:error summary)))
          (if-let [data (ex-data throwable)]
            (is (= data (:error-data summary)))
            (is (not (contains? summary :error-data)))))))))

;; Park signals (claude-12, 2026-10-09): a launcher parks on wm-click/end and
;; wm-debugger/stop rather than polling the status route.

(def ^:private test-debugger-stops (atom {}))

(defn- park-signal-resolver
  ([run! signals] (park-signal-resolver run! signals true))
  ([run! signals admit?]
   (let [base (resolver run! (fn [_] {:selected-policy-id "pi-test"}))]
     (fn [sym]
       (case sym
         futon3c.transport.http/complete-park-dep!
         (fn [dep-id result] (swap! signals conj [dep-id result]))
         futon2.aif.wm.debugger/!stops #'test-debugger-stops
         futon2.aif.full-loop-runtime/run-opportunity!
         (if admit? (base sym) run!)
         (base sym))))))

(def ^:private park-cast
  {:author "zai-2" :reviewer "codex-2" :repair-reviewer "codex-1"})

(deftest durable-acceptance-does-not-wait-for-runner-readiness
  (let [release (promise)
        entered (promise)
        issued (atom 0)
        raw-run! (fn [_]
                   (deliver entered true)
                   @release
                   {:attempt-id "attempt-pending" :outcome :incomplete})
        base (resolver raw-run! (fn [_] {:selected-policy-id "unused"}))
        pre-admission-resolver
        (fn [sym]
          (if (= sym 'futon2.aif.full-loop-runtime/run-opportunity!)
            raw-run!
            (base sym)))]
    (binding [service/*resolve-var* pre-admission-resolver
              service/*admission-wait-ms* 20]
      (let [answer (service/click!
                    (assoc park-cast :ordinary-click/issue!
                           (fn [& _] (swap! issued inc))))]
        (is (= true (deref entered 1000 false)))
        (is (= :queued (:status answer)))
        (is (= (:click-id answer) (:click-id (service/status))))
        (is (true? (:running? (service/status))))
        (is (= 1 @issued) "durable acceptance charges the ration exactly once")
        (deliver release true)
        (is (= :completed (:status (service/await-click! (:click-id answer)))))
        (is (= :incomplete (get-in (service/status) [:last-result :outcome])))))))

(deftest click-end-completes-generic-and-click-scoped-park-deps
  (let [signals (atom [])
        run! (fn [_] {:attempt-id "attempt-test" :outcome :grounded-change})]
    (binding [service/*resolve-var* (park-signal-resolver run! signals)]
      (let [{:keys [click-id]} (service/click! park-cast)]
        (service/await-click! click-id)
        (is (= [service/click-end-dep (str service/click-end-dep "/" click-id)]
               (mapv first @signals)))
        (is (= :grounded-change (:outcome (second (first @signals)))))
        (is (= click-id (:click-id (second (first @signals)))))))))

(deftest failed-click-completes-park-deps-with-its-error-kind
  (let [signals (atom [])
        run! (fn [_] (throw (ex-info "budget gone"
                                     {:error :ordinary-click-budget-exhausted})))]
    (binding [service/*resolve-var* (park-signal-resolver run! signals)]
      (let [click-id (try (:click-id (service/click! park-cast))
                          (catch Exception _ (:click-id (service/status))))]
        (service/await-click! click-id)
        (is (= [service/click-end-dep (str service/click-end-dep "/" click-id)]
               (mapv first @signals)))
        (is (= :service-failed (:outcome (second (first @signals)))))
        (is (= :ordinary-click-budget-exhausted
               (:error-kind (second (first @signals)))))
        (is (not (contains? (second (first @signals)) :error-data))
            "ex-data may carry throwables; the park result stays plain data")))))

(deftest debugger-stop-completes-park-deps-once-per-new-stop
  (let [signals (atom [])]
    (reset! test-debugger-stops {})
    (binding [service/*resolve-var* (park-signal-resolver (fn [_] nil) signals)]
      (is (= :installed (service/install-debugger-park-signal!)))
      (is (= :installed (service/install-debugger-park-signal!)) "idempotent")
      (swap! test-debugger-stops assoc "run-1"
             {:run-id "run-1" :phase :author :condition {:kind :ex-info}
              :decision (promise) :throwable (Exception. "x")})
      (swap! test-debugger-stops assoc-in ["run-1" :phase] :author)
      (swap! test-debugger-stops dissoc "run-1")
      (is (= [service/debugger-stop-dep (str service/debugger-stop-dep "/run-1")]
             (mapv first @signals)))
      (is (= {:run-id "run-1" :phase :author :condition {:kind :ex-info}
              :has-stop-value? false}
             (second (first @signals)))))))

(deftest debugger-stop-is-queryable-after-immediate-acceptance
  ;; The ration admission is a runner phase (futon2, 2026-10-09), so an
  ;; attached debugger can stop before admission. click! must answer with the
  ;; stop rather than hold its caller until a restart.
  (let [signals (atom [])
        release (promise)
        run! (fn [_]
               (swap! test-debugger-stops assoc "run-adm"
                      {:run-id "run-adm" :phase :admission
                       :condition {:kind :ex-info} :decision (promise)})
               @release
               (swap! test-debugger-stops dissoc "run-adm")
               {:attempt-id "attempt-test" :outcome :incomplete})]
    (reset! test-debugger-stops {})
    (binding [service/*resolve-var* (park-signal-resolver run! signals false)]
      (let [answer (deref (future (service/click!
                                   (assoc park-cast
                                          :ordinary-click/issue! (fn [& _]))))
                          5000 ::blocked)]
        (is (not= ::blocked answer))
        (is (= :queued (:status answer)))
        (is (= (:click-id answer) (:click-id (service/click-status (:click-id answer)))))
        (deliver release true)
        (service/await-click! (:click-id answer))
        (is (empty? (filter #(and (vector? %) (= ::service/pre-admission-stop (first %)))
                            (keys (.getWatches test-debugger-stops))))
            "the per-click watch is removed when the click ends")))))

(defn- report-card-binding [dir outcome]
  (let [path (.getAbsolutePath (io/file dir "click-run-binding-click-1.edn"))
        binding {:schema :wm-click-run-binding-v1
                 :click/id "click-1" :outcome outcome
                 :binding-status :verified
                 :run-id-observation {:status :present :value "run-1"}
                 :run-record (.getAbsolutePath (io/file dir "tick-run-record-run-1.edn"))
                 :path path :durability :confirmed}]
    (spit path (pr-str (dissoc binding :path :durability)))
    binding))

(deftest close-binding-generates-card-and-launches-publication
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "wm-report-card-success"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        close-binding (report-card-binding dir :completed)
        generated (atom []) launched (atom [])]
    (binding [service/*generate-report-card!*
              (fn [path] (swap! generated conj path) {:html "report-card.html"})
              service/*launch-report-card-publish!*
              (fn [run-id] (swap! launched conj run-id)
                {:id "bg-test" :pid 42 :out-file "/tmp/test"})]
      (let [before (slurp (:path close-binding))
            updated (service/publish-report-card-after-close! close-binding)
            sidecar (edn/read-string (slurp (io/file dir "report-card-click-1.edn")))]
        (is (= [(:run-record close-binding)] @generated))
        (is (= ["run-1"] @launched))
        (is (= :publication-launched (get-in updated [:report-card :status])))
        (is (= :publication-launched (get-in sidecar [:report-card :status])))
        (is (= before (slurp (:path close-binding)))
            "the verified click binding is never rewritten")
        (is (= :completed (:outcome (edn/read-string before))))))))

(deftest report-card-generator-failure-is-isolated-from-click-outcome
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "wm-report-card-failure"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        close-binding (report-card-binding dir :reviewer-falsifier-failed)
        launched? (atom false)]
    (binding [service/*generate-report-card!*
              (fn [_] (throw (ex-info "generator broke" {:reason :generator-broke})))
              service/*launch-report-card-publish!*
              (fn [_] (reset! launched? true))]
      (let [before (slurp (:path close-binding))
            updated (service/publish-report-card-after-close! close-binding)
            sidecar (edn/read-string (slurp (io/file dir "report-card-click-1.edn")))]
        (is (false? @launched?))
        (is (= {:status :failed :reason :generator-broke}
               (select-keys (:report-card updated) [:status :reason])))
        (is (= before (slurp (:path close-binding))))
        (is (= :reviewer-falsifier-failed (:outcome (edn/read-string before))))
        (is (= :failed (get-in sidecar [:report-card :status])))))))

(def ^:private click4-refusal
  "Click 4 (wm-click-fca1c921) disposition: route! refused, no run id."
  {:status :refused :reason :meta-self-heal/production-config-required
   :route :dedicated-meta-self-heal :ordinary-selection-called? false
   :absent-terms [:acceptance-test]})

(defn- summarize [click-id result]
  (#'service/result-summary click-id result "attempt-fallback"))

(deftest route-check-launches-for-refused-click-4-binding
  ;; Click 4 (wm-click-fca1c921): :binding-status :absent, which the
  ;; report-card hook skips; the route check is for exactly this binding.
  (let [calls (atom [])]
    (binding [service/*launch-route-check!*
              (fn [path click-id] (swap! calls conj [path click-id]) {:id "bg-test" :pid 42})]
      (let [summary (summarize "click-4" click4-refusal)
            binding-path (:run-binding summary)]
        (is (empty? @calls))
        (is (= :absent (:binding-status summary)))
        (is (nil? (:route-check summary)))
        (is (not (.exists (io/file (str binding-path ".route.edn"))))
            "the hook returns before the script writes any sidecar: it is never awaited")))))

(deftest route-check-launches-for-verified-click-2-binding-beside-report-card
  ;; Click 2 (wm-click-6cdf2a7c) closed with a verified run record.
  (let [dir service/*click-run-binding-dir*
        record (.getAbsolutePath (io/file dir "tick-run-record-run-2.edn"))
        _ (spit record (pr-str {:run/id "run-2" :click/id "click-2"}))
        calls (atom []) launched (atom [])]
    (binding [service/*generate-report-card!* (fn [_] {:html "card.html"})
              service/*launch-report-card-publish!*
              (fn [run-id] (swap! launched conj run-id) {:id "bg-card" :pid 41})
              service/*launch-route-check!*
              (fn [path click-id] (swap! calls conj [path click-id]) {:id "bg-test" :pid 42})]
      (let [summary (summarize "click-2" {:run/id "run-2" :run-record record :outcome :abstained})]
        (is (= :verified (:binding-status summary)))
        (is (empty? @calls))
        (is (= ["run-2"] @launched) "the report-card hook still runs")
        (is (nil? (:route-check summary)))))))

(deftest route-check-launch-failure-is-isolated-from-click-3-outcome
  ;; Click 3 (wm-click-2b1d568c) shape: refused at route!; the launch throws.
  (binding [service/*launch-route-check!*
            (fn [_ _] (throw (ex-info "launcher down" {})))]
    (let [refusal (assoc click4-refusal :outcome :refused
                         :absent-terms [:acceptance-run-port :diagnosis-evidence :acceptance-test])
          summary (summarize "click-3" refusal)
          binding-path (:run-binding summary)
          binding-bytes (slurp binding-path)
          launched (service/launch-route-check-after-close! {:path binding-path :click/id "click-3"})
          bytes-after-launch (slurp binding-path)
          sidecar (edn/read-string (slurp (str binding-path ".route.edn")))]
      (is (= :refused (:outcome summary)) "the click outcome survives the failed launch")
      (is (= {:click/id "click-3" :verdict :not-checked :reason "launch-failed"}
             (select-keys sidecar [:click/id :verdict :reason])))
      (is (= :failed (get-in launched [:route-check :status])))
      (is (= :refused (:outcome (edn/read-string binding-bytes))))
      (is (nil? (:route-check (edn/read-string binding-bytes)))
          "the click binding is not rewritten with the failure")
      (is (= binding-bytes bytes-after-launch)
          "the failed launch changed no byte of the close binding")
      (service/launch-route-check-after-close! {:path binding-path :click/id "click-3"})
      (is (= binding-bytes (slurp binding-path)) "byte-identical after a second failed launch"))))

(deftest run-id-uniqueness-scan-ignores-route-sidecars
  ;; L2b: the route certificate is written as click-run-binding-<id>.edn.route.edn,
  ;; which the scan's name filter would otherwise read as binding history and
  ;; throw on when half-written, failing the next click's binding write.
  (let [dir (io/file service/*click-run-binding-dir*)]
    (spit (io/file dir "click-run-binding-click-9.edn")
          (pr-str {:click/id "click-9" :run-id-observation {:value "run-9"}}))
    (spit (io/file dir "click-run-binding-click-9.edn.route.edn")
          "{:click/id \"click-9\" :verdict")
    (is (= ["click-9"] (#'service/existing-run-id-clicks dir "click-10" "run-9"))
        "the unreadable route sidecar is neither read nor counted")))

(deftest active-self-heal-missing-authority-refuses-before-ration-callback
  (let [issued (atom 0)
        resolver (fn [sym]
                   (case (str sym)
                     "futon2.aif.full-loop-runner/config" identity
                     "futon2.aif.meta-self-heal-production/production-readiness"
                     (fn [_] {:status :refused
                              :reason :meta-self-heal/production-config-required
                              :absent-terms [:repair-ports]})
                     nil))]
    (binding [service/*resolve-var* resolver]
      (let [failure (try
                      (service/click! {:ordinary-click/issue!
                                       (fn [& _] (swap! issued inc))})
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :meta-self-heal/production-config-required (:error failure)))
        (is (true? (:before-ration-spend? failure)))
        (is (zero? @issued))))))

(deftest ordinary-click-carries-f2a-self-heal-absence-to-the-runner
  ;; Historical pin: wm-click-07653f2a was refused before a run id after the
  ;; 08:19Z active-injury ledger; this is a hermetic F2a result-shape seam.
  (let [seen (atom nil)
        run! (fn [opts]
               (reset! seen opts)
               {:attempt-id "f2b-ordinary" :outcome :ordinary-selected})
        base (resolver run! (fn [_] {:selected-policy-id "ordinary-f2b"}))
        f2a-resolver (fn [sym]
                       (if (= sym 'futon2.aif.meta-self-heal-production/production-readiness)
                         (fn [_] {:status :ready
                                  :self-heal {:status :absent
                                              :absent-terms [:diagnosis-port
                                                             :acceptance-proposal-port
                                                             :acceptance-review-port]}})
                         (base sym)))]
    (binding [service/*resolve-var* f2a-resolver]
      (let [{:keys [click-id status]} (service/click! {:wm-agent-id scratch-agent-id})]
        (is (= :queued status))
        (is (= :completed (:status (service/await-click! click-id))))
        (is (= :ordinary (:meta-self-heal/readiness-route @seen)))
        (is (= {:status :absent
                :absent-terms [:diagnosis-port
                               :acceptance-proposal-port
                               :acceptance-review-port]}
               (:meta-self-heal-readiness @seen)))))))

(deftest ledger-unreadable-still-refuses-before-ordinary-ration
  (let [issued (atom 0)
        resolver (fn [sym]
                   (case (str sym)
                     "futon2.aif.full-loop-runner/config" identity
                     "futon2.aif.meta-self-heal-production/production-readiness"
                     (fn [_] {:status :refused
                              :reason :meta-self-heal/ledger-unreadable})
                     nil))]
    (binding [service/*resolve-var* resolver]
      (let [failure (try
                      (service/click! {:ordinary-click/issue!
                                       (fn [& _] (swap! issued inc))})
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= 503 (:status failure)))
        (is (= :meta-self-heal/ledger-unreadable (:error failure)))
        (is (zero? @issued))))))

(deftest configured-self-heal-ports-forward-the-acceptance-run-fn
  "F8 (click wm-click-2b1d568c, 2026-10-19): the adapter has supplied
   :meta-acceptance-run-fn since F4b and readiness has required it since F4a,
   but the service forwarded a fixed key list that omitted it, so every
   self-heal click refused :acceptance-run-port."
  (let [run-fn (fn [_] {:status :ran})
        id-fn (fn [repair-id attempt ordinal] (str repair-id attempt ordinal))
        resolved {:status :ready
                  :readiness {:status :ready :defects []}
                  :agency-invoke-fn identity :repair-enact-fn identity
                  :repair-retry-fn identity :meta-diagnosis-fn identity
                  :meta-acceptance-fn identity :meta-acceptance-review-fn identity
                  :meta-acceptance-run-fn run-fn
                  :retry-click-id-fn id-fn}
        configured (with-redefs [futon3c.wm.meta-self-heal-adapters/ports
                                 (fn [_] resolved)]
                     (#'service/configure-self-heal-ports {}))]
    (is (true? (:self-heal-concrete-ports? configured)))
    (is (= run-fn (:meta-acceptance-run-fn configured))
        "the run port reaches the runtime options")
    (is (identical? id-fn (:retry-click-id-fn configured))
        "M10b-3: the retry click minting function reaches the runtime options")
    (is (every? fn? (map configured [:meta-diagnosis-fn :meta-acceptance-fn
                                     :meta-acceptance-review-fn :agency-invoke-fn
                                     :repair-enact-fn :repair-retry-fn])))))

(deftest configured-self-heal-ports-forward-the-work-order-identities
  "Click wm-click-c78d0f27 (2026-10-19): the adapter resolved the cast and
   repository but the service forwarded only the port functions, so the work
   order reached prepare without sessions or a worktree. The identities ride
   with the ports; an explicit value in the click options wins and only nil
   or absent keys are filled."
  (let [identities {:author "wm-author" :reviewer "wm-reviewer"
                    :repair-reviewer "wm-repair-reviewer"
                    :author-session "wm-author/s1" :reviewer-session "wm-reviewer/s2"
                    :repair-reviewer-session "wm-repair-reviewer/s3"
                    :target-repository "/repo" :target-worktree "/repo"}
        resolved {:status :ready
                  :readiness {:status :ready :defects []}
                  :work-order-identities identities
                  :agency-invoke-fn identity :repair-enact-fn identity
                  :repair-retry-fn identity :meta-diagnosis-fn identity
                  :meta-acceptance-fn identity :meta-acceptance-review-fn identity
                  :meta-acceptance-run-fn identity}
        configured (with-redefs [futon3c.wm.meta-self-heal-adapters/ports
                                 (fn [_] resolved)]
                     (#'service/configure-self-heal-ports
                      {:author "codex-9" :target-worktree nil}))]
    (is (true? (:self-heal-concrete-ports? configured)))
    (is (= "codex-9" (:author configured)) "an explicit cast seat wins")
    (is (= "/repo" (:target-worktree configured)) "a nil key is filled")
    (is (= (dissoc identities :author)
           (select-keys configured (keys (dissoc identities :author)))))))
