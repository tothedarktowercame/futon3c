(ns futon3c.apm.countdown-control
  "Operator-stepped controller for the post-baseline f19--f27 countdown."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-postconditions :as postconditions]
            [futon3c.apm.campaign-stepper :as stepper]
            [futon3c.apm.countdown-manifest :as countdown-manifest]
            [futon3c.apm.countdown-pre-admission :as admission]
            [futon3c.apm.live-preflight-runtime :as live-preflight-runtime]
            [futon3c.apm.live-learning-phases :as live-learning-phases]
            [futon3c.apm.live-orchestration-contract :as orchestration-contract]
            [futon3c.apm.live-proof-phases :as live-proof-phases]
            [futon3c.apm.live-supervisor :as live-supervisor]
            [futon3c.apm.problem-projection :as problem-projection])
  (:import [java.nio.file Path]
           [java.time Instant]))

(def manifest-path "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")
(def contract-path "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")
(def state-directory "data/apm-campaigns/countdown-f19-f27-r4")
(def ledger-path "data/apm-campaigns/countdown-f19-f27-r4/ledger.edn")
(def certificate-directory "data/apm-campaigns/countdown-f19-f27-r4/certificates")
(def projection-directory "data/apm-campaigns/countdown-f19-f27-r4/projection")
(def problem-buffer-path "data/apm-campaigns/countdown-f19-f27-r4/problem-buffer.md")
(def preflight-state-path "data/apm-campaigns/countdown-f19-f27-r4/live/preflight.edn")
(def preparation-path
  "holes/labs/M-apm-demonstration/countdown-f19-live-preparation-v2.edn")
(def orchestration-path
  "holes/labs/M-apm-demonstration/countdown-live-orchestration-v1.edn")
(def control-branch "frame/18-control")
(def control-revision "d6f9ec2cfe622f518a423941f24819fa1a65fc5d")
(def ^:dynamic *control-root*
  (Path/of (System/getProperty "user.dir") (make-array String 0)))

(defn- control-path [path]
  (let [candidate (Path/of (str path) (make-array String 0))]
    (if (.isAbsolute candidate) candidate (.resolve *control-root* candidate))))

(defn- inputs []
  {:manifest (edn/read-string (slurp (str (control-path manifest-path))))
   :contract (edn/read-string (slurp (str (control-path contract-path))))})

(defn registration-body []
  (let [{:keys [manifest contract]} (inputs)
        units (subvec (:units manifest) 1)
        manifest-check (countdown-manifest/validate manifest)
        _ (when-not (:valid? manifest-check)
            (throw (ex-info "Countdown manifest failed executable validation"
                            manifest-check)))
        registered
        (mapv (fn [unit]
                (let [check (admission/validate
                             {:countdown-manifest manifest
                              :cycle-contract contract
                              :manifest-check manifest-check
                              :frame-id (:frame/id unit)})]
                  (when-not (:ok check)
                    (throw (ex-info "Countdown unit failed pre-admission" check)))
                  {:frame-id (:frame/id unit) :problem-id (:problem/id unit)
                   :arm (:arm unit) :registration-hash (:registration/hash check)
                   :harness-hash (get-in manifest [:apparatus :pin/id])}))
              units)]
    {:series :apm :manifest-hash (:manifest/id manifest)
     :phase-order (:phase-order contract)
     :block-plan [{:block-id (:block/id manifest) :ordinal 1 :units registered}]
     :obligation-plan
     (into {} (map (fn [[phase spec]]
                     [phase (select-keys spec [:kind :role :ordinal])])
                   (:phases contract)))
     :claims-required? true}))

(defn bootstrap! []
  (let [loaded (ledger/read-ledger (control-path ledger-path))]
    (cond
      (not (:ok loaded)) loaded
      (seq (:events loaded))
      {:ok (= "apm-countdown-r4" (get-in loaded [:projection :campaign/id]))
       :status :already-registered :projection (:projection loaded)}
      :else
      (let [body (registration-body)
            base {:event/seq 0 :event/type :campaign/registered
                  :event/campaign-id "apm-countdown-r4"
                  :event/actor "countdown-control"
                  :event/at (str (Instant/now)) :event/expected-version 0
                  :event/body body}
            event (assoc base :event/id (machine/ledger-digest [base]))
            empty-projection (machine/projection [])]
        (ledger/compare-and-append! (control-path ledger-path) 0
                                    (:ledger/digest empty-projection) event)))))

(defn- projection-sink [payload]
  (if (get-in payload [:certificate :active/frame])
    (problem-projection/project-latest!
     {:ledger-path (control-path ledger-path)
      :projection-directory (control-path projection-directory)
      :output-path (control-path problem-buffer-path) :expected-frame-id "f19"
      :expected-problem-id "a01J05"
      :buffer-sink problem-projection/emacs-buffer-sink})
    {:ok true :projected? false :reason :no-active-frame}))

(defn- gate-provider [{:keys [obligation]}]
  (let [{:keys [manifest contract]} (inputs)
        frame-id (get-in obligation [:obligation/action :frame-id])
        check (when frame-id
                (admission/validate {:countdown-manifest manifest
                                     :cycle-contract contract
                                     :frame-id frame-id}))
        evidence (if frame-id (:checks check)
                     {:manifest-content-addressed? true})]
    [{:gate/id :known-failing-regressions
      :gate/status (if (every? true? (vals evidence)) :pass :fail)
      :gate/evidence {:requirements
                      (mapv (fn [[id pass?]]
                              {:requirement/id id :actual pass? :pass? pass?})
                            evidence)}}]))

(defn- certified-handler [kind action]
  (let [phase (or (:phase action) kind)
        state-path (.resolve (control-path state-directory)
                             (str "live/" (name phase) ".edn"))
        state (live-preflight-runtime/read-state state-path)
        receipt (:receipt state)]
    (if (and (contains? #{:live-job-certified :preflight-certified}
                        (:state/type state))
             (= (:frame-id action) (:receipt/frame-id receipt))
             (= (:problem-id action) (:receipt/problem-id receipt)))
      {:ok true :certificate receipt}
      {:ok false :error/code :countdown-certified-phase-unavailable
       :finding {:kind kind :state/type (:state/type state)}})))

(defn- options []
  {:ledger-path (control-path ledger-path)
   :certificate-directory (control-path certificate-directory)
   :projection-directory (control-path projection-directory) :now-fn #(Instant/now)
   :observation-fn (fn [_] {:binding-response {:ok true :bound? false}
                            :jobs-response {:ok true :jobs []}})
   :gate-provider gate-provider :postcondition-fn postconditions/validate
   :project-fn projection-sink
   :handlers {:open-block (fn [action]
                            {:ok true :certificate
                             {:effect :countdown-block-opened
                              :block-id (:block-id action)}})
              :open-frame (fn [action]
                            {:ok true :certificate
                             {:effect :frame-admitted
                              :frame-id (:frame-id action)
                              :problem-id (:problem-id action)
                              :registration-hash (:registration-hash action)}})
              :preflight (partial certified-handler :preflight)
              :solve (partial certified-handler :solve)
              :verify (partial certified-handler :verify)
              :student-attempt (partial certified-handler :student-attempt)
              :guide-intervention (partial certified-handler :guide-intervention)
              :scribe-reduce (partial certified-handler :scribe-reduce)
              :close-frame (partial certified-handler :close-frame)}
   :actor "countdown-control"})

(defn inspect! [] (stepper/inspect! (options)))

(defn advance! [expected-kind]
  (let [boot (bootstrap!) inspection (inspect!)]
    (if-not (and (:ok boot) (:ok inspection)
                 (= :ready (:stepper/status inspection))
                 (= expected-kind
                    (get-in inspection [:obligation :obligation/action :kind])))
      {:ok false :error/code :countdown-step-precondition-failed
       :expected expected-kind :inspection inspection}
      (let [issued (stepper/issue-permit
                    {:report (:report inspection) :issuer "joe"
                     :issued-at (str (Instant/now))})]
        (if-not (:ok issued)
          issued
          (stepper/step!
           (assoc (options) :permit (:permit issued)
                  :trusted-permit-id (get-in issued [:permit :permit/id])
                  :trusted-issuer "joe")))))))

(defn live-preflight-inputs []
  (let [{:keys [manifest contract]} (inputs)
        unit (second (:units manifest))
        loaded (ledger/read-ledger (control-path ledger-path))
        projection (:projection loaded)
        response (live-preflight-runtime/http-json
                  "GET" "http://localhost:7070/api/alpha/agents/f19-proctor")
        agent (:agent response)
        metadata (:metadata agent)]
    {:contract contract
     :inputs
     {:ledger {:version (:campaign/version projection)
               :digest (:ledger/digest projection)
               :phase (get-in projection [:active/frame :phase])
               :claim (:active/claim projection)}
      :unit unit :role-card (get-in manifest [:apparatus :artifacts :proctor])
      :seat {:agent-id (:agent-id response) :type (some-> (:type agent) keyword)
             :frame-id (:frame-id metadata) :invoke-ready? (:invoke-ready? agent)}
      :timeouts {:request-timeout-ms 300000
                 :turn-timeout-ms
                 (get-in metadata [:effective-timeouts :turn-timeout-ms])}}
     :state-path (control-path preflight-state-path)}))

(defn run-live-preflight! []
  (live-preflight-runtime/run-live! (live-preflight-inputs)))

(defn live-proof-phase-inputs [action]
  (let [{:keys [manifest contract]} (inputs)
        unit (second (:units manifest))
        kind (:kind action)
        role (case kind :solve :solver :verify :proctor :preflight :proctor)
        prep (edn/read-string (slurp (str (control-path preparation-path))))
        workspace (get-in prep [:workspaces (if (= :solve kind) :solver :solver)])
        response (live-preflight-runtime/http-json
                  "GET" (str "http://localhost:7070/api/alpha/agents/f19-"
                             (name role)))
        agent (:agent response)
        metadata (:metadata agent)
        projection (:projection (ledger/read-ledger (control-path ledger-path)))
        solve-state (live-preflight-runtime/read-state
                     (.resolve (control-path state-directory) "live/solve.edn"))
        built (live-proof-phases/build-request
               {:kind kind
                :action (assoc action :timeouts {:request-ms 300000
                                                 :turn-ms 3600000})
                :ledger {:version (:campaign/version projection)
                         :digest (:ledger/digest projection)
                         :phase (get-in projection [:active/frame :phase])
                         :claim (:active/claim projection)}
                :unit unit :role-card (get-in manifest [:apparatus :artifacts role])
                :seat {:agent-id (:agent-id response)
                       :type (some-> (:type agent) keyword)
                       :frame-id (:frame-id metadata)
                       :invoke-ready? (:invoke-ready? agent)}
                :workspace workspace :solve-receipt (:receipt solve-state)})]
    (if-not (:ok built)
      built
      {:ok true :kind kind :contract contract :request (:request built)
       :state-path (.resolve (control-path state-directory)
                            (str "live/" (name kind) ".edn"))})))

(defn drive-live-proof-phase! [action]
  (if (= :preflight (:kind action))
    (run-live-preflight!)
    (let [phase-inputs (live-proof-phase-inputs action)]
      (if (:ok phase-inputs)
        (live-proof-phases/run-live! phase-inputs)
        phase-inputs))))

(defn- certified-receipts [contract]
  (into {}
        (keep (fn [phase]
                (let [state (live-preflight-runtime/read-state
                             (.resolve (control-path state-directory)
                                       (str "live/" (name phase) ".edn")))]
                  (when-let [receipt (:receipt state)] [phase receipt]))))
        (:phase-order contract)))

(defn live-learning-phase-inputs [action]
  (let [{:keys [manifest contract]} (inputs)
        unit (second (:units manifest))
        kind (:kind action)
        phase (:phase action)
        role (get live-learning-phases/role-for-kind kind)
        state-path (.resolve (control-path state-directory)
                             (str "live/" (name phase) ".edn"))
        existing (live-preflight-runtime/read-state state-path)
        prep (edn/read-string (slurp (str (control-path preparation-path))))
        response (live-preflight-runtime/http-json
                  "GET" (str "http://localhost:7070/api/alpha/agents/f19-"
                             (name role)))
        agent (:agent response)
        metadata (:metadata agent)
        projection (:projection (ledger/read-ledger (control-path ledger-path)))
        receipts (certified-receipts contract)
        built (when-not existing
                (live-learning-phases/build-request
                 {:contract contract :action action
                  :ledger {:digest (:ledger/digest projection)} :unit unit
                  :role-card (get-in manifest [:apparatus :artifacts role])
                  :seat {:agent-id (:agent-id response)
                         :type (some-> (:type agent) keyword)
                         :frame-id (:frame-id metadata)
                         :invoke-ready? (:invoke-ready? agent)}
                  :workspace (get-in prep [:workspaces :student])
                  :receipts receipts}))]
    (cond
      (and existing (map? (:request existing)))
      {:ok true :contract contract :action action :receipts receipts
       :request (:request existing) :state-path state-path}
      (:ok built)
      {:ok true :contract contract :action action :receipts receipts
       :request (:request built) :state-path state-path}
      :else built)))

(defn drive-live-learning-phase! [action]
  (let [phase-inputs (live-learning-phase-inputs action)]
    (if (:ok phase-inputs)
      (live-learning-phases/run-live! phase-inputs)
      phase-inputs)))

(defn launch-audit!
  "Validate complete executable wiring plus the exact continuation identity."
  [{:keys [agent session surface agency-base]
    :or {agency-base "http://localhost:7070"}}]
  (let [{:keys [manifest]} (inputs)
        spec-result (orchestration-contract/read-spec
                     (str (control-path orchestration-path)))
        head (shell/sh "git" "-C" (str *control-root*) "rev-parse" "HEAD")
        branch (shell/sh "git" "-C" (str *control-root*) "branch" "--show-current")
        ancestry (shell/sh "git" "-C" (str *control-root*)
                           "merge-base" "--is-ancestor" control-revision "HEAD")
        control-pinned? (and (zero? (:exit head)) (zero? (:exit branch))
                             (zero? (:exit ancestry))
                             (= control-branch (str/trim (:out branch))))
        contract-result
        (when (:ok spec-result)
          (orchestration-contract/validate
           {:spec (:spec spec-result) :registration-body (registration-body)
            :handlers (:handlers (options))
            :apparatus-artifacts (get-in manifest [:apparatus :artifacts])}))
        identity (when (and (string? agent) (not-empty agent))
                   (live-preflight-runtime/http-json
                    "GET" (str agency-base "/api/alpha/agents/" agent)))
        exact? (and (= 200 (:http/status identity)) (:ok identity)
                    (= agent (:agent-id identity))
                    (= session (get-in identity [:agent :session-id]))
                    (= surface "emacs-repl")
                    (true? (get-in identity [:agent :invoke-ready?])))]
    (cond
      (not (:ok spec-result)) spec-result
      (not control-pinned?)
      {:ok false :error/code :set-alight-control-root-not-pinned
       :finding {:root (str *control-root*) :expected-branch control-branch
                 :expected-revision control-revision
                 :observed-branch (str/trim (:out branch))
                 :observed-revision (str/trim (:out head))}}
      (not (:ok contract-result)) contract-result
      (not exact?)
      {:ok false :error/code :set-alight-continuation-identity-mismatch
       :finding {:expected {:agent agent :session session :surface surface}
                 :observed (select-keys identity
                                        [:http/status :ok :agent-id])
                 :observed-session (get-in identity [:agent :session-id])}}
      :else {:ok true :contract-audit contract-result
             :continuation {:agent agent :session session :surface surface}})))

(defn- f19-inspect! []
  (let [inspection (inspect!)
        next-frame (get-in inspection [:obligation :obligation/action :frame-id])]
    (if (and (:ok inspection) next-frame (not= "f19" next-frame))
      (assoc inspection :stepper/status :complete :completed-frame "f19")
      inspection)))

(defn- drive-live-action! [action]
  (cond
    (contains? #{:open-block :open-frame} (:kind action))
    (let [handled ((get-in (options) [:handlers (:kind action)]) action)]
      (if (:ok handled)
        {:ok true :status :certified :certificate (:certificate handled)}
        handled))

    (contains? #{:preflight :solve :verify} (:kind action))
    (drive-live-proof-phase! action)

    (contains? #{:student-attempt :guide-intervention :scribe-reduce :close-frame}
               (:kind action))
    (drive-live-learning-phase! action)

    :else
    {:ok false :error/code :set-alight-action-unsupported :action action}))

(defn- project-current! []
  (problem-projection/project-latest!
   {:ledger-path (control-path ledger-path)
    :projection-directory (control-path projection-directory)
    :output-path (control-path problem-buffer-path) :expected-frame-id "f19"
    :expected-problem-id "a01J05"
    :buffer-sink problem-projection/emacs-buffer-sink}))

(defn set-alight!
  "Run one durable f19 supervisor tick and arrange its exact continuation.

   Repeated calls are safe: persisted job tickets and Agency activation are
   idempotent, while the ledger remains the sole phase authority."
  ([continuation] (set-alight! continuation {}))
  ([{:keys [agent session surface agency-base control-root]}
    {:keys [launch-audit-fn inspect-fn drive-phase-fn advance-fn project-fn
            park-fn now-ms-fn]
     :or {now-ms-fn #(System/currentTimeMillis)}}]
   (binding [*control-root* (Path/of (str (or control-root *control-root*))
                                    (make-array String 0))]
    (let [identity {:agent agent :session session :surface surface
                   :control-root (str *control-root*)
                   :agency-base (or agency-base "http://localhost:7070")}
         payload (str "F19 SET-ALIGHT CONTINUATION: evaluate "
                      "(futon3c.apm.countdown-control/set-alight! "
                      (pr-str (select-keys identity
                                           [:agent :session :surface :control-root])) ").")
         park-default
         (fn [{:keys [awaiting] :as request}]
           (let [body (cond-> {:agent agent :session session :surface surface
                               :awaiting awaiting :payload (:payload request)}
                        (empty? awaiting)
                        (assoc :timer-due-ms (+ (long (now-ms-fn)) 500)))
                 response (live-preflight-runtime/http-json
                           "POST" (str (:agency-base identity) "/api/alpha/park") body)]
             {:ok (and (= 200 (:http/status response)) (:ok response))
              :response response}))]
     (live-supervisor/tick!
      {:launch-audit-fn (or launch-audit-fn #(launch-audit! identity))
       :inspect-fn (or inspect-fn f19-inspect!)
       :drive-phase-fn (or drive-phase-fn drive-live-action!)
       :advance-fn (or advance-fn (fn [kind _certificate] (advance! kind)))
       :project-fn (or project-fn project-current!)
       :park-fn (or park-fn park-default)
       :continuation-payload payload})))))
