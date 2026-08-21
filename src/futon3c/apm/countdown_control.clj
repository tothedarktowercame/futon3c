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
            [futon3c.apm.live-batch-supervisor :as live-batch-supervisor]
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
(def batch-cursor-path "data/apm-campaigns/countdown-f19-f27-r4/live/batch-cursor.edn")
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

(defn frame-unit [manifest frame-id]
  (some #(when (= frame-id (:frame/id %)) %) (:units manifest)))

(defn- preparation-path-for [frame-id]
  (if (= "f19" frame-id)
    preparation-path
    (str "holes/labs/M-apm-demonstration/countdown-" frame-id
         "-live-preparation-v2.edn")))

(defn- state-path-for [frame-id phase]
  (if (= "f19" frame-id)
    (.resolve (control-path state-directory) (str "live/" (name phase) ".edn"))
    (.resolve (control-path state-directory)
              (str "live/" frame-id "/" (name phase) ".edn"))))

(defn validate-frame-preparation
  "Bind a frame-scoped preparation to its exact manifest unit.

   Future preparations are content addressed. The already-certified f19 v2
   preparation predates that field and is accepted only for f19."
  [manifest unit preparation]
  (let [frame-id (:frame/id unit)
        problem-id (:problem/id unit)
        future? (not= "f19" frame-id)
        addressed? (= (:preparation/id preparation)
                      (machine/ledger-digest
                       [(dissoc preparation :preparation/id)]))
        findings
        (cond-> []
          (not= 2 (:preparation/version preparation))
          (conj :preparation-version-mismatch)
          (not= frame-id (:frame/id preparation))
          (conj :preparation-frame-mismatch)
          (not= problem-id (:problem/id preparation))
          (conj :preparation-problem-mismatch)
          (not= (:manifest/id manifest) (:manifest/id preparation))
          (conj :preparation-manifest-mismatch)
          (and future? (not addressed?))
          (conj :preparation-content-address-invalid)
          (some (fn [role]
                  (let [workspace (get-in preparation [:workspaces role])]
                    (or (not= frame-id (:frame/id workspace))
                        (not= problem-id (:problem/id workspace))
                        (not= role (:role workspace))
                        (not (string? (:workspace/path workspace)))
                        (not (string? (:workspace/id workspace)))
                        (not (string? (:branch workspace)))
                        (not (string? (:base-revision workspace))))))
                [:solver :student])
          (conj :preparation-workspace-mismatch)
          (some (fn [[role expected-type]]
                  (let [seat (get-in preparation [:seats role])]
                    (or (not= (str frame-id "-" (name role)) (:agent-id seat))
                        (not= expected-type (:type seat)))))
                {:solver :codex :student :zai :guide :claude
                 :proctor :codex :scribe :zai})
          (conj :preparation-seat-mismatch))]
    (if (seq findings)
      {:ok false :error/code :countdown-frame-preparation-invalid
       :frame-id frame-id :findings findings}
      {:ok true :frame-id frame-id :problem-id problem-id
       :preparation preparation})))

(defn- validate-live-workspaces [preparation]
  (let [findings
        (mapcat
         (fn [[role workspace]]
           (let [path (:workspace/path workspace)
                 branch (when (string? path)
                          (shell/sh "git" "-C" path "branch" "--show-current"))
                 ancestry (when (string? path)
                            (shell/sh "git" "-C" path "merge-base"
                                      "--is-ancestor" (:base-revision workspace)
                                      "HEAD"))]
             (cond-> []
               (not (and (string? path)
                         (.isDirectory (java.io.File. path))))
               (conj {:finding :workspace-path-missing :role role :path path})
               (not (zero? (or (:exit branch) 1)))
               (conj {:finding :workspace-git-unavailable :role role})
               (and branch (zero? (:exit branch))
                    (not= (:branch workspace) (str/trim (:out branch))))
               (conj {:finding :workspace-branch-mismatch :role role})
               (not (zero? (or (:exit ancestry) 1)))
               (conj {:finding :workspace-base-not-ancestor :role role}))))
         (:workspaces preparation))]
    (if (seq findings)
      {:ok false :error/code :countdown-frame-workspace-invalid
       :findings (vec findings)}
      {:ok true})))

(defn frame-context
  ([frame-id] (frame-context frame-id nil))
  ([frame-id preparation-provider]
   (let [{:keys [manifest contract]} (inputs)
         unit (frame-unit manifest frame-id)
         path (control-path (preparation-path-for frame-id))
         preparation
         (cond
           (fn? preparation-provider) (preparation-provider frame-id)
           (java.nio.file.Files/isRegularFile path (make-array java.nio.file.LinkOption 0))
           (edn/read-string (slurp (str path)))
           :else nil)]
     (cond
       (nil? unit) {:ok false :error/code :countdown-frame-not-in-manifest
                    :frame-id frame-id}
       (nil? preparation)
       {:ok false :error/code :countdown-frame-not-provisioned
        :frame-id frame-id :preparation-path (str path)}
       :else
       (let [validated (validate-frame-preparation manifest unit preparation)
             live-validation (when (and (:ok validated)
                                        (not (fn? preparation-provider)))
                               (validate-live-workspaces preparation))]
         (if (and (:ok validated) (or (nil? live-validation)
                                      (:ok live-validation)))
           {:ok true :manifest manifest :contract contract :unit unit
            :preparation preparation}
           (if-not (:ok validated) validated live-validation)))))))

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
  (if-let [frame (get-in payload [:certificate :active/frame])]
    (problem-projection/project-latest!
     {:ledger-path (control-path ledger-path)
      :projection-directory (control-path projection-directory)
      :output-path (control-path problem-buffer-path)
      :expected-frame-id (:frame-id frame)
      :expected-problem-id (:problem-id frame)
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
        state-path (state-path-for (:frame-id action) phase)
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

(defn advance!
  ([expected-kind] (advance! expected-kind nil))
  ([expected-kind batch-authority]
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
           (cond-> (assoc (options) :permit (:permit issued)
                          :trusted-permit-id (get-in issued [:permit :permit/id])
                          :trusted-issuer "joe")
             batch-authority
             (assoc :require-batch-permit? true
                    :batch-permit (:permit batch-authority)
                    :trusted-batch-permit-id
                    (:trusted-permit-id batch-authority)
                    :trusted-batch-permit-issuer
                    (:trusted-issuer batch-authority))))))))))

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
  (let [context (frame-context (:frame-id action))]
    (if-not (:ok context)
      context
      (let [{:keys [manifest contract unit preparation]} context
        kind (:kind action)
        role (case kind :solve :solver :verify :proctor :preflight :proctor)
        workspace (get-in preparation [:workspaces :solver])
        response (live-preflight-runtime/http-json
                  "GET" (str "http://localhost:7070/api/alpha/agents/"
                             (:frame/id unit) "-"
                             (name role)))
        agent (:agent response)
        metadata (:metadata agent)
        projection (:projection (ledger/read-ledger (control-path ledger-path)))
        solve-state (live-preflight-runtime/read-state
                     (state-path-for (:frame/id unit) :solve))
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
       :state-path (state-path-for (:frame/id unit) kind)})))))

(defn drive-live-proof-phase! [action]
  (let [phase-inputs (live-proof-phase-inputs action)]
    (if (:ok phase-inputs)
      (live-proof-phases/run-live! phase-inputs)
      phase-inputs)))

(defn- certified-receipts [contract frame-id]
  (into {}
        (keep (fn [phase]
                (let [state (live-preflight-runtime/read-state
                             (state-path-for frame-id phase))]
                  (when-let [receipt (:receipt state)] [phase receipt]))))
        (:phase-order contract)))

(defn live-learning-phase-inputs [action]
  (let [context (frame-context (:frame-id action))]
    (if-not (:ok context)
      context
      (let [{:keys [manifest contract unit preparation]} context
        kind (:kind action)
        phase (:phase action)
        role (get live-learning-phases/role-for-kind kind)
        state-path (state-path-for (:frame/id unit) phase)
        existing (live-preflight-runtime/read-state state-path)
        response (live-preflight-runtime/http-json
                  "GET" (str "http://localhost:7070/api/alpha/agents/"
                             (:frame/id unit) "-"
                             (name role)))
        agent (:agent response)
        metadata (:metadata agent)
        projection (:projection (ledger/read-ledger (control-path ledger-path)))
        receipts (certified-receipts contract (:frame/id unit))
        built (when-not existing
                (live-learning-phases/build-request
                 {:contract contract :action action
                  :ledger {:digest (:ledger/digest projection)} :unit unit
                  :role-card (get-in manifest [:apparatus :artifacts role])
                  :seat {:agent-id (:agent-id response)
                         :type (some-> (:type agent) keyword)
                         :frame-id (:frame-id metadata)
                         :invoke-ready? (:invoke-ready? agent)}
                  :workspace (get-in preparation [:workspaces :student])
                  :receipts receipts}))]
    (cond
      (and existing (map? (:request existing)))
      {:ok true :contract contract :action action :receipts receipts
       :request (:request existing) :state-path state-path}
      (:ok built)
      {:ok true :contract contract :action action :receipts receipts
       :request (:request built) :state-path state-path}
      :else built)))))

(defn drive-live-learning-phase! [action]
  (let [phase-inputs (live-learning-phase-inputs action)]
    (if (:ok phase-inputs)
      (live-learning-phases/run-live! phase-inputs)
      phase-inputs)))

(defn launch-audit!
  "Validate complete executable wiring plus the exact continuation identity."
  [{:keys [agent session surface agency-base target-frame]
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
        preparation-result (frame-context (or target-frame "f19"))
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
      (not (:ok preparation-result)) preparation-result
      (not exact?)
      {:ok false :error/code :set-alight-continuation-identity-mismatch
       :finding {:expected {:agent agent :session session :surface surface}
                 :observed (select-keys identity
                                        [:http/status :ok :agent-id])
                 :observed-session (get-in identity [:agent :session-id])}}
      :else {:ok true :contract-audit contract-result
             :continuation {:agent agent :session session :surface surface}})))

(defn- frame-inspect! [target-frame]
  (let [inspection (inspect!)
        next-frame (get-in inspection [:obligation :obligation/action :frame-id])]
    (if (and (:ok inspection) next-frame (not= target-frame next-frame))
      (assoc inspection :stepper/status :complete :completed-frame target-frame)
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

(defn- project-current! [frame-id]
  (let [{:keys [manifest]} (inputs)
        unit (frame-unit manifest frame-id)
        loaded (ledger/read-ledger (control-path ledger-path))
        active (get-in loaded [:projection :active/frame])]
    (cond
      (not (:ok loaded)) loaded
      (nil? unit)
      {:ok false :error/code :countdown-frame-not-in-manifest :frame-id frame-id}
      (nil? active)
      {:ok true :projected? false :reason :frame-closed :frame-id frame-id}
      (not= frame-id (:frame-id active))
      {:ok false :error/code :countdown-projection-frame-mismatch
       :expected frame-id :actual (:frame-id active)}
      :else
      (problem-projection/project-latest!
       {:ledger-path (control-path ledger-path)
        :projection-directory (control-path projection-directory)
        :output-path (control-path problem-buffer-path) :expected-frame-id frame-id
        :expected-problem-id (:problem/id unit)
        :buffer-sink problem-projection/emacs-buffer-sink}))))

(defn set-alight!
  "Run one durable f19 supervisor tick and arrange its exact continuation.

   Repeated calls are safe: persisted job tickets and Agency activation are
   idempotent, while the ledger remains the sole phase authority."
  ([continuation] (set-alight! continuation {}))
  ([{:keys [agent session surface agency-base control-root target-frame
            batch-authority]}
    {:keys [launch-audit-fn inspect-fn drive-phase-fn advance-fn project-fn
            park-fn now-ms-fn continuation-payload]
     :or {now-ms-fn #(System/currentTimeMillis)}}]
   (binding [*control-root* (Path/of (str (or control-root *control-root*))
                                    (make-array String 0))]
    (let [target-frame (or target-frame "f19")
          identity {:agent agent :session session :surface surface
                   :control-root (str *control-root*)
                   :target-frame target-frame
                   :batch-authority batch-authority
                   :agency-base (or agency-base "http://localhost:7070")}
         payload (or continuation-payload
                     (str (str/upper-case target-frame)
                      " SET-ALIGHT CONTINUATION: evaluate "
                      "(futon3c.apm.countdown-control/set-alight! "
                      (pr-str (select-keys identity
                                           [:agent :session :surface :control-root
                                            :target-frame :batch-authority])) ")."))
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
       :inspect-fn (or inspect-fn #(frame-inspect! target-frame))
       :drive-phase-fn (or drive-phase-fn drive-live-action!)
       :advance-fn (or advance-fn
                       (fn [kind _certificate]
                         (advance! kind batch-authority)))
       :project-fn (or project-fn #(project-current! target-frame))
       :park-fn (or park-fn park-default)
       :continuation-payload payload})))))

(defn set-alight-batch!
  "Drive one tick of an explicitly bounded, batch-permitted frame chain.

   FRAME-TICK-FN is the pinned per-frame adapter. Keeping it explicit prevents
   the chaining layer from pretending that an unprovisioned future frame is
   runnable. The durable campaign ledger remains the action counter."
  ([authority] (set-alight-batch! authority {}))
  ([{:keys [start-frame end-frame permit trusted-permit-id trusted-issuer
            control-root agent session surface agency-base] :as authority}
    {:keys [frame-tick-fn inspect-fn cursor-read-fn cursor-persist-fn
            continue-fn now-ms-fn]
     :or {now-ms-fn #(System/currentTimeMillis)}}]
   (binding [*control-root* (Path/of (str (or control-root *control-root*))
                                    (make-array String 0))]
     (let [manifest (:manifest (inputs))
           cursor-path (control-path batch-cursor-path)
           resumable-authority (assoc authority :control-root (str *control-root*))
           payload (str "COUNTDOWN BATCH CONTINUATION: evaluate "
                        "(futon3c.apm.countdown-control/set-alight-batch! "
                        (pr-str resumable-authority) ").")
           batch-authority {:permit permit :trusted-permit-id trusted-permit-id
                            :trusted-issuer trusted-issuer}
           park-next
           (fn []
             (let [response
                   (live-preflight-runtime/http-json
                    "POST" (str (or agency-base "http://localhost:7070")
                                "/api/alpha/park")
                    {:agent agent :session session :surface surface :awaiting []
                     :timer-due-ms (+ (long (now-ms-fn)) 500)
                     :payload payload})]
               {:ok (and (= 200 (:http/status response)) (:ok response))
                :response response}))]
       (live-batch-supervisor/tick!
        {:units (subvec (:units manifest) 1)
         :start-frame start-frame :end-frame end-frame :permit permit
         :trusted-permit-id trusted-permit-id :trusted-issuer trusted-issuer
         :actor "countdown-control"
         :inspect-fn (or inspect-fn inspect!)
         :frame-tick-fn
         (or frame-tick-fn
             (fn [{:keys [frame-id]}]
               (set-alight!
                {:agent agent :session session :surface surface
                 :agency-base agency-base :control-root (str *control-root*)
                 :target-frame frame-id :batch-authority batch-authority}
                {:continuation-payload payload})))
         :cursor-read-fn (or cursor-read-fn
                             #(live-preflight-runtime/read-state cursor-path))
         :cursor-persist-fn
         (or cursor-persist-fn
             #(live-preflight-runtime/atomic-persist! cursor-path %))
         :continue-fn (or continue-fn park-next)
         :authority (dissoc authority :permit)})))))
