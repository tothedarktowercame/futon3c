(ns futon3c.apm.countdown-control
  "Operator-stepped controller for the post-baseline f19--f27 countdown."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-executor :as executor]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-postconditions :as postconditions]
            [futon3c.apm.campaign-runner :as runner]
            [futon3c.apm.campaign-stepper :as stepper]
            [futon3c.apm.countdown-manifest :as countdown-manifest]
            [futon3c.apm.countdown-pre-admission :as admission]
            [futon3c.apm.generated-contract :as generated-contract]
            [futon3c.apm.authority-port :as authority-port]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-preflight-runtime :as live-preflight-runtime]
            [futon3c.apm.live-learning-phases :as live-learning-phases]
            [futon3c.apm.live-launch-preparation :as live-preparation]
            [futon3c.apm.live-promotion :as live-promotion]
            [futon3c.apm.live-batch-supervisor :as live-batch-supervisor]
            [futon3c.apm.live-orchestration-contract :as orchestration-contract]
            [futon3c.apm.live-proof-phases :as live-proof-phases]
            [futon3c.apm.live-regulator :as live-regulator]
            [futon3c.apm.live-supervisor :as live-supervisor]
            [futon3c.apm.jit-queue-coordinator :as jit-coordinator]
            [futon3c.apm.memory-snapshot :as memory-snapshot]
            [futon3c.apm.promotion-pipeline :as promotion-pipeline]
            [futon3c.apm.frame-cycle-handlers :as frame-cycle-handlers]
            [futon3c.apm.analyst-campaign :as analyst-campaign]
            [futon3c.apm.problem-projection :as problem-projection]
            [futon3c.apm.open-problem-queue :as open-problem-queue]
            [futon3c.apm.problem-queue-supervisor :as problem-queue]
            [futon3c.apm.queued-frame-adapter :as queued-frame-adapter]
            [futon3c.apm.queued-frame-terminal :as queued-frame-terminal]
            [futon3c.apm.series-terminal :as series-terminal]
            [futon3c.apm.solver-progress-rollover :as progress-rollover]
            [futon3c.apm.workspace-lifecycle :as workspace-lifecycle]
            [futon3c.apm.qualification :as qualification])
  (:import [java.nio.file Path]
           [java.time Instant]))

(def ^:dynamic manifest-path "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")
(def ^:dynamic contract-path "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")
(def ^:dynamic generated-contract-path
  "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v3.json")
(def ^:dynamic qualification-report-path
  "data/apm-validation/qualification-report-v1.edn")
(def ^:dynamic state-directory "data/apm-campaigns/countdown-f19-f27-r4")
(def ^:dynamic ledger-path "data/apm-campaigns/countdown-f19-f27-r4/ledger.edn")
(def ^:dynamic certificate-directory "data/apm-campaigns/countdown-f19-f27-r4/certificates")
(def ^:dynamic projection-directory "data/apm-campaigns/countdown-f19-f27-r4/projection")
(def ^:dynamic problem-buffer-path "data/apm-campaigns/countdown-f19-f27-r4/problem-buffer.md")
(def ^:dynamic problem-buffer-name "*problem*")
(def ^:dynamic preflight-state-path "data/apm-campaigns/countdown-f19-f27-r4/live/preflight.edn")
(def ^:dynamic batch-cursor-path "data/apm-campaigns/countdown-f19-f27-r4/live/batch-cursor.edn")
(def ^:dynamic regulator-state-path "data/apm-campaigns/countdown-f19-f27-r4/live/regulator.edn")
(def ^:dynamic problem-queue-state-path
  "data/apm-campaigns/problem-queue/live/queue.edn")
(def ^:dynamic analyst-state-path "data/apm-campaigns/countdown-f19-f27-r4/analyst/state.edn")
(def ^:dynamic preparation-path
  "holes/labs/M-apm-demonstration/countdown-f19-live-preparation-v2.edn")
(def orchestration-path
  "holes/labs/M-apm-demonstration/countdown-live-orchestration-v1.edn")
(def control-branch "master")
;; The canonical shared JVM must run master containing the retained-revision
;; qualification repair. Later master commits remain valid by ancestry.
(def control-revision "11a2f9401a39e17ac3695b2e70e4d0e3b9d71858")
(def machine-regulator-id "countdown-regulator")
(defonce ^:private machine-regulator-capability (Object.))
(declare inputs)

(def f20-one-off-config
  {:manifest-path "holes/labs/M-apm-demonstration/f20-one-off-manifest-v1.edn"
   :state-directory "data/apm-campaigns/f20-one-off-v1"
   :ledger-path "data/apm-campaigns/f20-one-off-v1/ledger.edn"
   :certificate-directory "data/apm-campaigns/f20-one-off-v1/certificates"
   :projection-directory "data/apm-campaigns/f20-one-off-v1/projection"
   :problem-buffer-path "data/apm-campaigns/f20-one-off-v1/problem-buffer.md"
   :preflight-state-path "data/apm-campaigns/f20-one-off-v1/live/preflight.edn"
   :batch-cursor-path "data/apm-campaigns/f20-one-off-v1/live/batch-cursor.edn"
   :regulator-state-path "data/apm-campaigns/f20-one-off-v1/live/regulator.edn"
   :preparation-path "holes/labs/M-apm-demonstration/f20-one-off-live-preparation-v1.edn"})

(def f21-one-off-config
  {:manifest-path "holes/labs/M-apm-demonstration/f21-one-off-manifest-v1.edn"
   :contract-path "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"
   :state-directory "data/apm-campaigns/f21-one-off-v2"
   :ledger-path "data/apm-campaigns/f21-one-off-v2/ledger.edn"
   :certificate-directory "data/apm-campaigns/f21-one-off-v2/certificates"
   :projection-directory "data/apm-campaigns/f21-one-off-v2/projection"
   :problem-buffer-path "data/apm-campaigns/f21-one-off-v2/problem-buffer.md"
   :preflight-state-path "data/apm-campaigns/f21-one-off-v2/live/preflight.edn"
   :batch-cursor-path "data/apm-campaigns/f21-one-off-v2/live/batch-cursor.edn"
   :regulator-state-path "data/apm-campaigns/f21-one-off-v2/live/regulator.edn"
   :analyst-state-path "data/apm-campaigns/f21-one-off-v2/analyst/state.edn"
   :preparation-path "holes/labs/M-apm-demonstration/f21-one-off-live-preparation-v1.edn"})

(def f22-one-off-config
  {:manifest-path "holes/labs/M-apm-demonstration/f22-one-off-manifest-v1.edn"
   :contract-path "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"
   :state-directory "data/apm-campaigns/f22-one-off-v1"
   :ledger-path "data/apm-campaigns/f22-one-off-v1/ledger.edn"
   :certificate-directory "data/apm-campaigns/f22-one-off-v1/certificates"
   :projection-directory "data/apm-campaigns/f22-one-off-v1/projection"
   :problem-buffer-path "data/apm-campaigns/f22-one-off-v1/problem-buffer.md"
   :preflight-state-path "data/apm-campaigns/f22-one-off-v1/live/preflight.edn"
   :batch-cursor-path "data/apm-campaigns/f22-one-off-v1/live/batch-cursor.edn"
   :regulator-state-path "data/apm-campaigns/f22-one-off-v1/live/regulator.edn"
   :analyst-state-path "data/apm-campaigns/f22-one-off-v1/analyst/state.edn"
   :preparation-path "holes/labs/M-apm-demonstration/f22-one-off-live-preparation-v1.edn"})

(defmacro ^:private with-campaign [config & body]
  `(let [config# (or ~config {})]
     (binding [manifest-path (or (:manifest-path config#) manifest-path)
               contract-path (or (:contract-path config#) contract-path)
               generated-contract-path (or (:generated-contract-path config#)
                                           generated-contract-path)
               qualification-report-path
               (or (:qualification-report-path config#) qualification-report-path)
               state-directory (or (:state-directory config#) state-directory)
               ledger-path (or (:ledger-path config#) ledger-path)
               certificate-directory (or (:certificate-directory config#) certificate-directory)
               projection-directory (or (:projection-directory config#) projection-directory)
               problem-buffer-path (or (:problem-buffer-path config#) problem-buffer-path)
               problem-buffer-name (or (:problem-buffer-name config#) problem-buffer-name)
               preflight-state-path (or (:preflight-state-path config#) preflight-state-path)
               batch-cursor-path (or (:batch-cursor-path config#) batch-cursor-path)
               regulator-state-path (or (:regulator-state-path config#) regulator-state-path)
               problem-queue-state-path
               (or (:problem-queue-state-path config#) problem-queue-state-path)
               analyst-state-path (or (:analyst-state-path config#) analyst-state-path)
               preparation-path (or (:preparation-path config#) preparation-path)]
       ~@body)))

(defn- machine-regulator-authorized? [regulator-id capability]
  (and (string? regulator-id)
       (str/starts-with? regulator-id (str machine-regulator-id ":"))
       (identical? machine-regulator-capability capability)))

(defn- scoped-regulator-id []
  (str machine-regulator-id ":" (get-in (inputs) [:manifest :campaign/id])))
(def ^:dynamic *control-root*
  (Path/of (System/getProperty "user.dir") (make-array String 0)))

(defn- control-path [path]
  (let [candidate (Path/of (str path) (make-array String 0))]
    (if (.isAbsolute candidate) candidate (.resolve *control-root* candidate))))

(defn- dispatch-card [card]
  (let [checked (authority-port/require-path
                 {:control-root (str *control-root*)}
                 :role-card (:path card))]
    (if (:ok checked)
      {:ok true :card (assoc card :path (:path checked))}
      checked)))

(defn- inputs []
  (let [manifest (edn/read-string (slurp (str (control-path manifest-path))))
        legacy (edn/read-string (slurp (str (control-path contract-path))))]
    (if-not (= :apm-complete-frame-cycle-v2 (:contract/id legacy))
      {:manifest manifest :contract legacy}
      (let [result (generated-contract/validate-round-trip
                    (str (control-path generated-contract-path)) legacy)]
        (when-not (:ok result)
          (throw (ex-info "Lean-generated campaign contract rejected" result)))
        (let [generated (:contract result)]
          {:manifest manifest
           ;; Receipt schemas and phase requires/produces remain EDN-owned.
           ;; The executable ordering and numerical policy are Lean-owned.
           :contract (assoc legacy
                            :phase-order (mapv keyword (:phase-order generated))
                            :generated/bounds (:bounds generated)
                            :generated/dispatch-policy (:dispatch-policy generated)
                            :generated/terminal-policy (:terminal-policy generated)
                            :generated/source generated-contract-path)
           :generated/contract generated})))))

(defn- generated-bound [contract bound fallback]
  (or (get-in contract [:generated/bounds bound]) fallback))

(defn- generated-terminal-budgets [contract]
  (get-in contract [:generated/dispatch-policy :role-terminal-budgets]))

(defn frame-unit [manifest frame-id]
  (some #(when (= frame-id (:frame/id %)) %) (:units manifest)))

(defn- preparation-path-for [frame-id]
  (if (or (= :one-off (:manifest/scope (:manifest (inputs))))
          (= "f19" frame-id))
    preparation-path
    (str "holes/labs/M-apm-demonstration/countdown-" frame-id
         "-live-preparation-v2.edn")))

(defn- state-path-for [frame-id phase]
  (if (= :one-off (:manifest/scope (:manifest (inputs))))
    (.resolve (control-path state-directory) (str "live/" (name phase) ".edn"))
    (if (= "f19" frame-id)
    (.resolve (control-path state-directory) (str "live/" (name phase) ".edn"))
    (.resolve (control-path state-directory)
              (str "live/" frame-id "/" (name phase) ".edn")))))

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
                 :proctor :codex :promotion-proctor :codex :scribe :zai
                 :zai-scribe :zai})
          (conj :preparation-seat-mismatch))]
    (if (seq findings)
      {:ok false :error/code :countdown-frame-preparation-invalid
       :frame-id frame-id :findings findings}
      {:ok true :frame-id frame-id :problem-id problem-id
       :preparation preparation})))

(defn- validate-live-workspaces
  ([preparation]
   (let [terminal-path (.resolve (control-path state-directory)
                                 "terminal/frame-terminal.edn")
         terminal (when (java.nio.file.Files/isRegularFile
                         terminal-path (make-array java.nio.file.LinkOption 0))
                    (edn/read-string (slurp (str terminal-path))))
         receipt-directory (.resolve (control-path state-directory)
                                     "terminal/workspaces")]
     (validate-live-workspaces
      preparation (:workspace/terminal-heads terminal)
      (fn [workspace terminal-head]
        (workspace-lifecycle/retirement-status
         {:lease workspace :terminal-head terminal-head
          :receipt-directory receipt-directory})))))
  ([preparation terminal-heads retirement-status-fn]
   (let [findings
        (mapcat
         (fn [[role workspace]]
           (let [path (:workspace/path workspace)
                 path-present? (and (string? path)
                                    (.isDirectory (java.io.File. path)))
                 retirement (when-not path-present?
                              (retirement-status-fn
                               workspace (get terminal-heads role)))
                 retired? (and (:ok retirement)
                               (= :already-retired (:status retirement)))
                 branch (when (string? path)
                          (shell/sh "git" "-C" path "branch" "--show-current"))
                 ancestry (when (string? path)
                            (shell/sh "git" "-C" path "merge-base"
                                      "--is-ancestor" (:base-revision workspace)
                                      "HEAD"))]
             (cond-> []
               (and (not path-present?) (not retired?))
               (conj {:finding :workspace-path-missing :role role :path path})
               (and (not retired?) (not (zero? (or (:exit branch) 1))))
               (conj {:finding :workspace-git-unavailable :role role})
               (and (not retired?) branch (zero? (:exit branch))
                    (not= (:branch workspace) (str/trim (:out branch))))
               (conj {:finding :workspace-branch-mismatch :role role})
               (and (not retired?) (not (zero? (or (:exit ancestry) 1))))
               (conj {:finding :workspace-base-not-ancestor :role role})
               (and retirement (not (:ok retirement)))
               (conj {:finding :workspace-retirement-replay-invalid
                      :role role :evidence retirement}))))
         (:workspaces preparation))]
    (if (seq findings)
      {:ok false :error/code :countdown-frame-workspace-invalid
       :findings (vec findings)}
      {:ok true}))))

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
        units (if (= :one-off (:manifest/scope manifest))
                (:units manifest)
                (subvec (:units manifest) 1))
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
  (let [{:keys [manifest contract]} (inputs)
        loaded (ledger/read-ledger (control-path ledger-path))
        projection (:projection loaded)
        registration-matches?
        (and (= (:campaign/id manifest) (:campaign/id projection))
             (= (:manifest/id manifest) (:campaign/manifest-hash projection))
             (= (:phase-order contract) (:campaign/phase-order projection)))]
    (cond
      (not (:ok loaded)) loaded
      (seq (:events loaded))
      (if registration-matches?
        {:ok true :status :already-registered :projection projection}
        {:ok false :error/code :countdown-registration-mismatch
         :finding {:expected {:campaign/id (:campaign/id manifest)
                              :manifest-hash (:manifest/id manifest)
                              :phase-order (:phase-order contract)}
                   :observed {:campaign/id (:campaign/id projection)
                              :manifest-hash (:campaign/manifest-hash projection)
                              :phase-order (:campaign/phase-order projection)}}})
      :else
      (let [body (registration-body)
            base {:event/seq 0 :event/type :campaign/registered
                  :event/campaign-id (:campaign/id (:manifest (inputs)))
                  :event/actor "countdown-control"
                  :event/at (str (Instant/now)) :event/expected-version 0
                  :event/body body}
            event (assoc base :event/id (machine/ledger-digest [base]))
            empty-projection (machine/projection [])]
        (ledger/compare-and-append! (control-path ledger-path) 0
                                    (:ledger/digest empty-projection) event)))))

(defn- projection-sink [payload]
  (if-let [frame (get-in payload [:certificate :active/frame])]
    (let [current (live-preflight-runtime/read-state
                   (.resolve (control-path projection-directory) "latest.edn"))
          ledger-digest (get-in payload [:certificate :ledger/digest])]
      (if (= ledger-digest (:ledger/digest current))
        {:ok true :projected? false :reason :current-ledger-already-published}
        (problem-projection/project-latest!
         {:ledger-path (control-path ledger-path)
          :projection-directory (control-path projection-directory)
          :output-path (control-path problem-buffer-path)
          :buffer-name problem-buffer-name
          :expected-frame-id (:frame-id frame)
          :expected-problem-id (:problem-id frame)
          :buffer-sink problem-projection/emacs-buffer-sink})))
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
    (if (and (contains? #{:live-job-certified :preflight-certified
                          :promotion-certified}
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
              :close-block (fn [action]
                             {:ok true :certificate
                              {:effect :countdown-block-closed
                               :block-id (:block-id action)}})
              :close-campaign (fn [_action]
                                {:ok true :certificate
                                 {:effect :countdown-campaign-closed}})
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
        loaded (ledger/read-ledger (control-path ledger-path))
        projection (:projection loaded)
        frame-id (get-in projection [:active/frame :frame-id])
        unit (frame-unit manifest frame-id)
        problem (:problem unit)
        unit (assoc-in unit [:problem :repository]
                       (countdown-manifest/qualification-checkout-path
                        (:repository problem) (:revision problem)))
        response (live-preflight-runtime/http-json
                  "GET" (str "http://localhost:7070/api/alpha/agents/"
                             frame-id "-proctor"))
        agent (:agent response)
        metadata (:metadata agent)
        card (dispatch-card (get-in manifest [:apparatus :artifacts :proctor]))]
    (if-not (:ok card)
      card
      {:contract contract
     :inputs
     {:ledger {:version (:campaign/version projection)
               :digest (:ledger/digest projection)
               :phase (get-in projection [:active/frame :phase])
               :claim (:active/claim projection)}
      :unit unit :role-card (:card card)
      :seat {:agent-id (:agent-id response) :type (some-> (:type agent) keyword)
             :frame-id (:frame-id metadata) :invoke-ready? (:invoke-ready? agent)}
      :timeouts {:request-timeout-ms
                 (generated-bound contract :zai-request-timeout-ms 300000)
                 :turn-timeout-ms
                 (generated-bound contract :seat-turn-timeout-ms
                                  (get-in metadata
                                          [:effective-timeouts
                                           :turn-timeout-ms]))}}
     :state-path (control-path preflight-state-path)})))

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
        card (dispatch-card (get-in manifest [:apparatus :artifacts role]))
        checkpoint-card (when (= :solve kind)
                          (dispatch-card
                           (get-in manifest [:apparatus :artifacts
                                             :solver-restrategize])))
        built (live-proof-phases/build-request
               {:kind kind
                :action (assoc action :timeouts
                               {:request-ms (generated-bound
                                             contract :zai-request-timeout-ms 300000)
                                :turn-ms (generated-bound
                                          contract :seat-turn-timeout-ms 3600000)})
                :ledger {:version (:campaign/version projection)
                         :digest (:ledger/digest projection)
                         :phase (get-in projection [:active/frame :phase])
                         :claim (:active/claim projection)}
                :unit unit :role-card (:card card)
                :checkpoint-role-card
                (:card checkpoint-card)
                :terminal-budget (get (generated-terminal-budgets contract) role)
                :seat {:agent-id (:agent-id response)
                       :type (some-> (:type agent) keyword)
                       :frame-id (:frame-id metadata)
                       :invoke-ready? (:invoke-ready? agent)}
                :workspace workspace :solve-receipt (:receipt solve-state)})]
    (cond
      (not (:ok card)) card
      (and checkpoint-card (not (:ok checkpoint-card))) checkpoint-card
      (not (:ok built))
      built
      :else {:ok true :kind kind :contract contract :request (:request built)
       :terminal-budget (get (generated-terminal-budgets contract) role)
       :max-rounds (generated-bound contract :solver-max-rounds 50)
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
                  (when-let [receipt (:receipt state)] [(keyword phase) receipt]))))
        (:phase-order contract)))

(defn student-attempt-inputs
  "Stable, receipt-derived end-of-frame mining inputs. Job traces remain
  controller-owned and are fetched through their immutable Agency job ids."
  [contract frame-id]
  (->> (:phase-order contract)
       (filter #(str/starts-with? (name %) "student-attempt-"))
       (keep (fn [phase]
               (let [state (live-preflight-runtime/read-state
                            (state-path-for frame-id phase))
                     receipt (:receipt state)
                     job-id (:receipt/job-id receipt)]
                 (when (string? job-id)
                   {:phase phase
                    :job-id job-id
                    :job-trace-ref (str "http://localhost:7070/api/alpha/invoke/jobs/"
                                        job-id)
                    :repair-job-ids
                    (vec (distinct (keep identity
                                         [(:terminal-repair/original-job-id state)])))
                    :memory-use (:receipt/memory-use receipt)
                    :failure-account (:receipt/failure-account receipt)}))))
       vec))

(defn live-learning-phase-inputs [action]
  (let [context (frame-context (:frame-id action))]
    (if-not (:ok context)
      context
      (let [{:keys [manifest contract unit preparation]} context
        kind (:kind action)
        phase (:phase action)
        role (if (and (= :scribe-reduce kind) (= :scribe-reduce phase))
               :zai-scribe
               (get live-learning-phases/role-for-kind kind))
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
        ;; The latest reviewed snapshot: a Guide union when one was
        ;; published, else the Solver promotion.
        promotion (when (= :student-attempt kind)
                    (frame-cycle-handlers/latest-snapshot-receipt
                     receipts (or (:ordinal action)
                                  (get-in contract [:phases phase :ordinal])
                                  1)))
        snapshot-access
        (when (and (= :student-attempt kind) promotion)
          (memory-snapshot/verify-student-access
           {:path (:receipt/snapshot-path promotion)
            :expected (:receipt/snapshot-digest promotion)
            :frame-id (:frame/id unit) :problem-id (:problem/id unit)
            :accessible-memory-ids (:receipt/reviewed-memory-ids promotion)}))
        card (dispatch-card (get-in manifest [:apparatus :artifacts role]))
        built (live-learning-phases/build-request
               {:contract contract :action action
                :ledger {:digest (:ledger/digest projection)} :unit unit
                :role-card (:card card)
                :seat {:agent-id (:agent-id response)
                       :type (some-> (:type agent) keyword)
                       :frame-id (:frame-id metadata)
                       :invoke-ready? (:invoke-ready? agent)}
                :workspace (get-in preparation [:workspaces :student])
                :receipts receipts :snapshot-access snapshot-access
                :seat-role role
                :student-attempt-inputs
                (when (= :scribe-reduce phase)
                  (student-attempt-inputs contract (:frame/id unit)))
                :terminal-budgets (generated-terminal-budgets contract)
                :turn-timeout-ms (generated-bound
                                  contract :seat-turn-timeout-ms 3600000)})]
    (cond
      (not (:ok card)) card
      (and existing (map? (:request existing)))
      {:ok true :contract contract :action action :receipts receipts
       :manifest manifest :unit unit :preparation preparation
       :request (:request existing) :state-path state-path}
      (:ok built)
      {:ok true :contract contract :action action :receipts receipts
       :manifest manifest :unit unit :preparation preparation
       :request (:request built) :state-path state-path}
      :else built)))))

(defn learning-regime-audit
  "V1 is a preserved baseline. V2 claims learning and therefore requires the
  two-promotion graph plus pinned campaign-level Analyst identity."
  [contract manifest preparation]
  (if-not (= :apm-complete-frame-cycle-v2 (:contract/id contract))
    {:ok true :regime :baseline-v1}
    (let [analyst-card (get-in manifest [:apparatus :artifacts :analyst])
          analyst-seat (get-in preparation [:seats :analyst])
          registration
          (analyst-campaign/register
           {:campaign-id (:campaign/id manifest)
            :analyst-seat (:agent-id analyst-seat)
            :analyst-card-path (:path analyst-card)
            :analyst-card-blob (:blob analyst-card)})
          checks
          {:promote-solver-before-student?
           (< (.indexOf (:phase-order contract) :promote-solver)
              (.indexOf (:phase-order contract) :student-attempt-1))
           :students-require-snapshot?
           (every? #(contains? (get-in contract [:phases % :requires])
                               :solver-memory-snapshot)
                   [:student-attempt-1 :student-attempt-2 :student-attempt-3])
           :analyst-card-pinned?
           (and (string? (:path analyst-card)) (string? (:blob analyst-card)))
           :analyst-seat-pinned? (string? (:agent-id analyst-seat))
           :analyst-tenure-registered? (:ok registration)}
          reviewer-card (get-in manifest [:apparatus :artifacts
                                          :promotion-proctor])
          reviewer-seat (get-in preparation [:seats :promotion-proctor])
          measurement-proctor-seat (get-in preparation [:seats :proctor])
          checks (assoc checks
                        :promotion-proctor-card-pinned?
                        (and (string? (:path reviewer-card))
                             (string? (:blob reviewer-card)))
                        :promotion-reviewer-distinct?
                        (and (string? (:agent-id reviewer-seat))
                             (not= (:agent-id reviewer-seat)
                                   (get-in preparation [:seats :scribe
                                                        :agent-id]))
                             (not= (:agent-id reviewer-seat)
                                   (:agent-id measurement-proctor-seat))))
          failed (into #{} (keep (fn [[k v]] (when-not v k))) checks)]
      (if (seq failed)
        {:ok false :error/code :learning-regime-incomplete
         :checks checks :failed failed}
        {:ok true :regime :two-promotion-v2 :checks checks
         :analyst-state (:state registration)}))))

(defn- promotion-review-request
  [{:keys [manifest contract unit preparation request]}]
  (let [card-result (dispatch-card
                     (get-in manifest [:apparatus :artifacts
                                       :promotion-proctor]))
        card (:card card-result)
        seat (get-in preparation [:seats :promotion-proctor])
        body {:dispatch/type :promotion-review
              :phase :promote-solver
              :role :promotion-proctor
              :agent-id (:agent-id seat)
              :frame-id (:frame/id unit)
              :problem-id (:problem/id unit)
              :ledger-digest (:ledger-digest request)
              :role-card-path (:path card)
              :role-card-blob (:blob card)
              :input-receipt-ids (:input-receipt-ids request)
              :base-problem-blob (:base-problem-blob request)
              :problem-path (:problem-path request)
              :solver-final-head (:solver-final-head request)
              :terminal-budget (get (generated-terminal-budgets contract)
                                    :promotion-proctor)
              :turn-timeout-ms (get-in preparation
                                       [:seat-policy :turn-timeout-ms])}]
    (if (:ok card-result)
      (assoc body :dispatch/id (machine/ledger-digest [body]))
      card-result)))

(defn- nested-snapshot-receipts [value]
  (cond
    (map? value)
    (concat (when (string? (:receipt/snapshot-path value)) [value])
            (mapcat nested-snapshot-receipts (vals value)))
    (sequential? value) (mapcat nested-snapshot-receipts value)
    :else []))

(defn campaign-prior-memories
  "Read prior frames from the durable queue order and each frame ledger's last
  snapshot receipt. This deliberately does not discover frames or snapshots by
  directory globbing or filename order."
  ([] (campaign-prior-memories (control-path problem-queue-state-path)))
  ([queue-path]
   (let [queue-path (Path/of (str queue-path) (make-array String 0))
         queue-state (live-preflight-runtime/read-state queue-path)
         campaign-root (.getParent queue-path)
         campaign-name (some-> campaign-root .getFileName str)]
     (reduce
      (fn [{:keys [candidates dropped] :as acc} completed-frame]
        (let [frame-id (:frame/id completed-frame)
              problem-id (:problem/id completed-frame)
              ledger-path (.resolve campaign-root
                                    (str campaign-name "-" frame-id "/ledger.edn"))
              history (ledger/read-ledger ledger-path)
              receipt (when (:ok history)
                        (->> (:events history)
                             reverse
                             (mapcat nested-snapshot-receipts)
                             first))
              snapshot-path (:receipt/snapshot-path receipt)
              snapshot (when (string? snapshot-path)
                         (try (edn/read-string (slurp snapshot-path))
                              (catch Throwable _ nil)))
              memories (:snapshot/memories snapshot)]
          (if (vector? memories)
            (assoc acc :candidates
                   (into candidates
                         (map #(assoc % :provenance
                                      {:frame-id frame-id :problem-id problem-id}))
                         memories))
            (assoc acc :dropped
                   (conj dropped {:frame-id frame-id :problem-id problem-id
                                  :finding (if (:ok history)
                                             :prior-snapshot-unreadable
                                             :prior-frame-ledger-unreadable)})))))
      {:candidates [] :dropped []}
      (or (:completed queue-state) [])))))

(defn- publish-promotion!
  [{:keys [contract action receipts request]}
   {:keys [candidates deposit reviewer reviews]}]
  (let [prior (campaign-prior-memories)
        own (mapv #(assoc % :provenance
                          {:frame-id (:frame-id action)
                           :problem-id (:problem-id action)})
                  candidates)
        published
        (memory-snapshot/publish-cumulative!
         {:frame-id (:frame-id action)
          :problem-id (:problem-id action)
          :prior-candidates (:candidates prior)
          :own-candidates own
          :path (.resolve (control-path state-directory)
                          (str "snapshots/" (:frame-id action)
                               "-solver-memory.edn"))
          :evidence-visible? memory-snapshot/candidate-visible?})]
    (if-not (:ok published)
      published
      (let [snapshot (:snapshot published)
            snapshot-memories (:snapshot/memories snapshot)
            own-ids (set (map :memory-id own))
            retained-prior (remove #(contains? own-ids (:memory-id %))
                                   snapshot-memories)
            accounting
            (promotion-pipeline/validate-extension-publication-accounting
             reviews retained-prior snapshot-memories)
            prior-dropped (into (vec (:dropped prior)) (:prior-dropped published))
            body {:receipt/type :solver-promotion
                  :receipt/frame-id (:frame-id action)
                  :receipt/problem-id (:problem-id action)
                  :receipt/input-receipt-ids (:input-receipt-ids request)
                  :receipt/lanes (or (:lanes deposit) (:lanes-run deposit) [])
                  :receipt/dispositions (or (:dispositions deposit)
                                            (:rejections deposit) [])
                  :receipt/promotion-reviews reviews
                  :receipt/prior-dropped prior-dropped
                  :receipt/snapshot-id (:snapshot/id snapshot)
                  :receipt/snapshot-digest (:snapshot/digest snapshot)
                  :receipt/snapshot-path (:path published)
                  :receipt/reviewed-memory-ids
                  (mapv :memory-id (:snapshot/memories snapshot))
                  :receipt/independent-review? (not= (:depositor deposit)
                                                     reviewer)}
            receipt (assoc body :receipt/id (machine/ledger-digest [body]))
            checked (frame-cycle-handlers/validate-completion
                     contract action receipt receipts)]
        (cond
          (not (:ok accounting)) accounting
          (:ok checked) {:ok true :receipt receipt}
          :else checked)))))

(defn- publish-zai-scribe-promotion!
  "Independently review the Student-mined candidates, publish their exact
  snapshot, and certify the ordinary end-of-frame scribe receipt."
  [{:keys [contract action receipts request]}
   {:keys [candidates deposit reviewer reviews]}]
  (let [prior (frame-cycle-handlers/latest-snapshot-receipt receipts 4)
        prior-path (:receipt/snapshot-path prior)
        prior-memories (if (string? prior-path)
                         (try
                           (:snapshot/memories
                            (edn/read-string (slurp prior-path)))
                           (catch Throwable _ ::unreadable))
                         [])
        current (map #(assoc % :provenance
                             {:frame-id (:frame-id action)
                              :problem-id (:problem-id action)})
                     candidates)
        union (when (vector? prior-memories)
                (->> (concat prior-memories current)
                     (reduce (fn [acc m] (assoc acc (:memory-id m) m)) {})
                     vals vec))
        published
        (when (vector? union)
        (memory-snapshot/publish!
         {:frame-id (:frame-id action) :problem-id (:problem-id action)
          :candidates union
          :path (.resolve (control-path state-directory)
                          (str "snapshots/" (:frame-id action)
                               "-student-mined-memory.edn"))
          :evidence-visible? memory-snapshot/candidate-visible?}))]
    (cond
      (not (vector? union))
      {:ok false :error/code :zai-scribe-prior-snapshot-unreadable
       :path prior-path}
      (not (:ok published))
      published
      :else
      (let [snapshot (:snapshot published)
            accounting
            (promotion-pipeline/validate-extension-publication-accounting
             reviews prior-memories (:snapshot/memories snapshot))
            body {:receipt/type :scribe-reduce
                  :receipt/frame-id (:frame-id action)
                  :receipt/problem-id (:problem-id action)
                  :receipt/input-receipt-ids (:input-receipt-ids request)
                  :receipt/lanes (or (:lanes deposit) [])
                  :receipt/dispositions (or (:dispositions deposit) [])
                  :receipt/promotion-reviews reviews
                  :receipt/snapshot-id (:snapshot/id snapshot)
                  :receipt/snapshot-digest (:snapshot/digest snapshot)
                  :receipt/snapshot-path (:path published)
                  :receipt/reviewed-memory-ids
                  (mapv :memory-id (:snapshot/memories snapshot))
                  :receipt/independent-review? (not= (:depositor deposit)
                                                     reviewer)}
            receipt (assoc body :receipt/id (machine/ledger-digest [body]))
            checked (frame-cycle-handlers/validate-completion
                     contract action receipt receipts)]
        (cond
          (not (:ok accounting)) accounting
          (not (:receipt/independent-review? body))
          {:ok false :error/code :zai-scribe-reviewer-is-depositor}
          (:ok checked) {:ok true :receipt receipt}
          :else checked)))))

(defn- guide-review-state-path [state-path]
  (let [path (Path/of (str state-path) (make-array String 0))
        file (str (.getFileName path))]
    (.resolveSibling path (str/replace file #"\.edn$" "-review.edn"))))

(defn- publish-guide-promotion!
  "Publish the union of the prior reviewed snapshot with the Guide candidates
  the promotion Proctor approved. Every prior memory is re-validated and
  re-checked against the substrate, so the union is as fresh as a first
  snapshot; an identical republish is idempotent."
  [{:keys [action request]} {:keys [candidates deposit reviewer reviews]}]
  (let [prior (:prior-snapshot request)
        prior-path (when (string? (:snapshot-path prior)) (:snapshot-path prior))
        prior-memories (if prior-path
                         (try
                           (:snapshot/memories
                            (edn/read-string (slurp prior-path)))
                           (catch Throwable _ ::unreadable))
                         [])
        ordinal (get-in request [:intervention-ordinal])
        current (map #(assoc % :provenance
                             {:frame-id (:frame-id action)
                              :problem-id (:problem-id action)})
                     candidates)
        union (when (vector? prior-memories)
                (->> (concat prior-memories current)
                     (reduce (fn [acc m] (assoc acc (:memory-id m) m)) {})
                     vals vec))]
    (if-not (vector? union)
      {:ok false :error/code :guide-promotion-prior-snapshot-unreadable
       :path prior-path}
      (let [published
            (memory-snapshot/publish!
             {:frame-id (:frame-id action)
              :problem-id (:problem-id action)
              :candidates union
              :path (.resolve (control-path state-directory)
                              (str "snapshots/" (:frame-id action)
                                   "-guide-" ordinal "-memory.edn"))
              :evidence-visible? memory-snapshot/candidate-visible?})]
        (if-not (:ok published)
          published
          (let [snapshot (:snapshot published)
                accounting
                (promotion-pipeline/validate-extension-publication-accounting
                 reviews prior-memories (:snapshot/memories snapshot))
                body {:receipt/type :guide-promotion
                      :receipt/frame-id (:frame-id action)
                      :receipt/problem-id (:problem-id action)
                      :receipt/intervention-ordinal ordinal
                      :receipt/prior-snapshot prior
                      :receipt/promotion-reviews reviews
                      :receipt/snapshot-id (:snapshot/id snapshot)
                      :receipt/snapshot-digest (:snapshot/digest snapshot)
                      :receipt/snapshot-path (:path published)
                      :receipt/reviewed-memory-ids
                      (mapv :memory-id (:snapshot/memories snapshot))
                      :receipt/independent-review? (not= (:depositor deposit)
                                                         reviewer)}]
            (cond
              (not (:ok accounting)) accounting
              (not (:receipt/independent-review? body))
              {:ok false :error/code :guide-promotion-reviewer-is-depositor}
              :else
              {:ok true
               :receipt (assoc body :receipt/id
                               (machine/ledger-digest [body]))})))))))

(defn drive-live-learning-phase! [action]
  (let [phase-inputs (live-learning-phase-inputs action)]
    (if (:ok phase-inputs)
      (case (:kind action)
        :scribe-reduce
        (if (= :promote-solver (:phase action))
          (live-promotion/run-live!
           {:state-path (:state-path phase-inputs)
            :control-root (str *control-root*)
            :deposit-request (:request phase-inputs)
            :reviewer-request (promotion-review-request phase-inputs)
            :publish-fn #(publish-promotion! phase-inputs %)})
          (live-promotion/run-live!
           {:state-path (:state-path phase-inputs)
            :control-root (str *control-root*)
            :deposit-request (:request phase-inputs)
            :reviewer-request (promotion-review-request phase-inputs)
            :publish-fn #(publish-zai-scribe-promotion! phase-inputs %)}))

        :guide-intervention
        (let [review-path (guide-review-state-path (:state-path phase-inputs))]
          (live-learning-phases/run-live!
           (assoc phase-inputs :guide-promotion
                  {:state-path review-path
                   :run-fn
                   #(live-promotion/run-live!
                     {:state-path review-path
                      :control-root (str *control-root*)
                      :deposit-request (:request phase-inputs)
                      :reviewer-request (promotion-review-request phase-inputs)
                      :publish-fn (fn [published]
                                    (publish-guide-promotion! phase-inputs
                                                              published))})})))

        (live-learning-phases/run-live! phase-inputs))
      phase-inputs)))

(defn record-analyst-wake!
  "Persist the v2 post-close Analyst obligation. Dispatch is deliberately a
  separate campaign action; this boundary only makes the wake durable."
  [frame-id close-receipt]
  (let [{:keys [manifest contract preparation] :as context}
        (frame-context frame-id)]
    (cond
      (not (:ok context)) context
      (not= :apm-complete-frame-cycle-v2 (:contract/id contract))
      {:ok true :status :baseline-v1-no-analyst-transition}
      :else
      (let [path (control-path analyst-state-path)
            existing (live-preflight-runtime/read-state path)
            audit (learning-regime-audit contract manifest preparation)
            state (or existing (:analyst-state audit))
            wake (when (:ok audit)
                   (analyst-campaign/wake-after-close state close-receipt))]
        (cond
          (not (:ok audit)) audit
          (not (:ok wake)) wake
          :else
          (let [persisted (live-preflight-runtime/atomic-persist!
                           path (:state wake))]
            (if (:ok persisted)
              (assoc wake :durable? true :state-path (str path))
              {:ok false :error/code :analyst-wake-persistence-failed})))))))

(defn qualification-audit []
  (let [contract (:contract (inputs))]
    (if-not (= :apm-complete-frame-cycle-v2 (:contract/id contract))
      {:ok true :status :legacy-contract-not-qualified}
      (let [path (control-path qualification-report-path)]
        (if-not (.isFile (.toFile path))
          {:ok false :error/code :apm-qualification-report-missing
           :path (str path)}
          (qualification/validate-report
           (edn/read-string (slurp (str path)))
           (str (control-path generated-contract-path))))))))

(defn dry-run-v2-launch []
  (let [{:keys [contract]} (inputs)
        qualification (qualification-audit)
        registration (registration-body)
        dispatch-policy (:generated/dispatch-policy contract)
        terminal-policy (:generated/terminal-policy contract)
        role-budgets (:role-terminal-budgets dispatch-policy)
        policy-audit
        {:all-live-roles-bounded?
         (and (= #{:solver :student :guide :scribe :proctor
                   :promotion-proctor :analyst}
                 (set (keys role-budgets)))
              (every? #(every? pos-int? (vals %)) (vals role-budgets)))
         :collection-persisted? (:terminal-collection-persisted dispatch-policy)
         :collection-before-missing?
         (:terminal-collection-before-missing-observation dispatch-policy)
         :valid-submission-collected?
         (:terminal-collection-required dispatch-policy)
         :student-only-alternate?
         (and (:missing-observation-student-only dispatch-policy)
              (= "controller" (:missing-observation-author terminal-policy))
              (false? (:missing-observation-may-impersonate-student
                       terminal-policy)))
         :solved-partial-bankable?
         (and (:solved-partial-bankable terminal-policy)
              (:bankable-solved-successor-eligible terminal-policy))}
        policies-pass? (every? true? (vals policy-audit))]
    {:ok (and (= :apm-complete-frame-cycle-v2 (:contract/id contract))
              (:ok qualification)
              (= (:phase-order contract) (:phase-order registration))
              policies-pass?)
     :dispatches [] :qualification qualification
     :registration registration :policy-audit policy-audit
     :historical-state-mutations [] :reference-fixture :f25-frozen}))

(defn launch-audit!
  "Validate complete executable wiring plus the exact continuation identity."
  [{:keys [agent session surface agency-base target-frame regulator-id
           regulator-capability]
    :or {agency-base "http://localhost:7070"}}]
  (let [{:keys [manifest contract]} (inputs)
        qualification-result (qualification-audit)
        loaded-ledger (ledger/read-ledger (control-path ledger-path))
        ledger-projection (:projection loaded-ledger)
        registration-matches?
        (and (:ok loaded-ledger)
             (= (:campaign/id manifest) (:campaign/id ledger-projection))
             (= (:manifest/id manifest)
                (:campaign/manifest-hash ledger-projection))
             (= (:phase-order contract)
                (:campaign/phase-order ledger-projection)))
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
        learning-result (when (:ok preparation-result)
                          (learning-regime-audit
                           (:contract preparation-result) manifest
                           (:preparation preparation-result)))
        machine-regulator? (machine-regulator-authorized?
                            regulator-id regulator-capability)
        identity (when (and (not machine-regulator?)
                            (string? agent) (not-empty agent))
                   (live-preflight-runtime/http-json
                    "GET" (str agency-base "/api/alpha/agents/" agent)))
        exact? (or machine-regulator?
                   (and (= 200 (:http/status identity)) (:ok identity)
                        (= agent (:agent-id identity))
                        (= session (get-in identity [:agent :session-id]))
                        (= surface "emacs-repl")
                        (true? (get-in identity [:agent :invoke-ready?]))))]
    (cond
      (not (:ok spec-result)) spec-result
      (not (:ok qualification-result)) qualification-result
      (not control-pinned?)
      {:ok false :error/code :set-alight-control-root-not-pinned
       :finding {:root (str *control-root*) :expected-branch control-branch
                 :expected-revision control-revision
                 :observed-branch (str/trim (:out branch))
                 :observed-revision (str/trim (:out head))}}
      (not (:ok contract-result)) contract-result
      (not registration-matches?)
      {:ok false :error/code :set-alight-registration-mismatch
       :finding {:expected {:campaign/id (:campaign/id manifest)
                            :manifest-hash (:manifest/id manifest)
                            :phase-order (:phase-order contract)}
                 :observed {:campaign/id (:campaign/id ledger-projection)
                            :manifest-hash
                            (:campaign/manifest-hash ledger-projection)
                            :phase-order
                            (:campaign/phase-order ledger-projection)}}}
      (not (:ok preparation-result)) preparation-result
      (not (:ok learning-result)) learning-result
      (not exact?)
      {:ok false :error/code :set-alight-continuation-identity-mismatch
       :finding {:expected {:agent agent :session session :surface surface}
                 :observed (select-keys identity
                                        [:http/status :ok :agent-id])
                 :observed-session (get-in identity [:agent :session-id])}}
      :else {:ok true :contract-audit contract-result
             :qualification qualification-result
             :learning-regime learning-result
             :continuation (if machine-regulator?
                             {:mode :machine :regulator-id regulator-id}
                             {:mode :agent :agent agent :session session
                              :surface surface})})))

(defn- frame-inspect! [target-frame]
  (let [inspection (inspect!)
        next-frame (get-in inspection [:obligation :obligation/action :frame-id])]
    (if (and (:ok inspection) next-frame (not= target-frame next-frame))
      (assoc inspection :stepper/status :complete :completed-frame target-frame)
      inspection)))

(defn- solver-projection-progress [solve-state active-phase]
  (let [certified? (= :live-job-certified (:state/type solve-state))
        completed (+ (count (:rounds solve-state)) (if certified? 1 0))
        contract (:contract (inputs))
        max-rounds (or (:budget/max-rounds solve-state)
                       (get-in solve-state [:active :request :solver/max-rounds])
                       (generated-bound contract :solver-max-rounds 50))
        active-round (when (and (= :solve active-phase) (not certified?))
                       (get-in solve-state [:active :request :solver/round]))
        checkpoint-next (when (and (not certified?) (< completed max-rounds))
                          (let [every (generated-bound
                                      contract :solver-checkpoint-every 10)]
                            (* every (inc (quot completed every)))))]
    {:rounds/completed completed
     :rounds/max max-rounds
     :round/active active-round
     :checkpoint/next (when (and checkpoint-next
                                 (<= checkpoint-next max-rounds))
                        checkpoint-next)}))

(defn- projection-phase-job-state [phase-state]
  (if (= :solver-rounds (:state/type phase-state))
    (:active phase-state)
    phase-state))

(defn- projection-operation [frame-id active-phase phase-job-state]
  (cond
    (= :live-job-dispatched (:state/type phase-job-state))
    (let [request (:request phase-job-state)]
      {:status :waiting-for-terminal-result
       :role (or (:role request)
                 (case active-phase
                   :preflight :proctor
                   :solve :solver
                   :verify :proctor
                   nil))
       :agent-id (:agent-id request)
       :job-id (get-in phase-job-state [:ticket :job-id])})

    (and (= :promotion (:state/type phase-job-state))
         (string? (:job phase-job-state)))
    (let [role (case (:stage phase-job-state)
                 :deposit :scribe
                 :independent-review :promotion-proctor
                 nil)]
      {:status :waiting-for-terminal-result
       :role role
       :agent-id (when role (str frame-id "-" (name role)))
       :job-id (:job phase-job-state)})

    :else nil))

(defn- drive-live-action! [action]
  (cond
    (contains? #{:open-block :open-frame :close-block :close-campaign}
               (:kind action))
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
  (let [checkpoint (runner/checkpoint!
                    (options) {:checkpoint/stage :live-projection-refresh})
        {:keys [manifest contract]} (inputs)
        unit (frame-unit manifest frame-id)
        loaded (ledger/read-ledger (control-path ledger-path))
        active (get-in loaded [:projection :active/frame])
        phase-state (when active
                      (let [path (state-path-for frame-id (:phase active))
                            base (live-preflight-runtime/read-state path)
                            ;; While a Guide deposit is under independent
                            ;; review, the operator-visible job is the
                            ;; reviewer's, not the finished Guide turn.
                            review (when (= :guide-intervention
                                            (get-in contract
                                                    [:phases (:phase active)
                                                     :kind]))
                                     (live-preflight-runtime/read-state
                                      (guide-review-state-path path)))]
                        (if (and (= :promotion (:state/type review))
                                 (not= :live-job-certified (:state/type base)))
                          review
                          base)))
        solve-state (live-preflight-runtime/read-state
                     (state-path-for frame-id :solve))
        solver-progress (solver-projection-progress solve-state (:phase active))
        phase-job-state (projection-phase-job-state phase-state)
        phase-request (:request phase-job-state)
        operation-mismatch?
        (and (= :live-job-dispatched (:state/type phase-job-state))
             (or (not= (get-in loaded [:projection :ledger/digest])
                       (:ledger-digest phase-request))
                 (not= frame-id (:frame-id phase-request))
                 (not= (:problem/id unit) (:problem-id phase-request))
                 (not= (:phase active) (:phase phase-request))))
        operation (projection-operation frame-id (:phase active)
                                        phase-job-state)]
    (cond
      (not (:ok checkpoint))
      {:ok false :error/code :countdown-projection-checkpoint-failed
       :checkpoint checkpoint}
      (not (:ok loaded)) loaded
      (nil? unit)
      {:ok false :error/code :countdown-frame-not-in-manifest :frame-id frame-id}
      (nil? active)
      {:ok true :projected? false :reason :frame-closed :frame-id frame-id}
      (not= frame-id (:frame-id active))
      {:ok false :error/code :countdown-projection-frame-mismatch
       :expected frame-id :actual (:frame-id active)}
      operation-mismatch?
      {:ok false :error/code :countdown-projection-operation-mismatch
       :frame-id frame-id :phase (:phase active)}
      :else
      (problem-projection/project-latest!
       {:ledger-path (control-path ledger-path)
        :projection-directory (control-path projection-directory)
        :output-path (control-path problem-buffer-path) :expected-frame-id frame-id
        :expected-problem-id (:problem/id unit)
        :buffer-name problem-buffer-name
        :operation operation
        :solver-progress solver-progress
        :buffer-sink problem-projection/emacs-buffer-sink}))))

(defn set-alight!
  "Run one durable f19 supervisor tick and arrange its exact continuation.

   Repeated calls are safe: persisted job tickets and Agency activation are
   idempotent, while the ledger remains the sole phase authority."
  ([continuation] (set-alight! continuation {}))
  ([{:keys [agent session surface agency-base control-root target-frame
            batch-authority regulator-id regulator-capability campaign-config]}
    {:keys [launch-audit-fn inspect-fn drive-phase-fn advance-fn project-fn
            park-fn now-ms-fn continuation-payload]
     :or {now-ms-fn #(System/currentTimeMillis)}}]
   (with-campaign campaign-config
    (binding [*control-root* (Path/of (str (or control-root *control-root*))
                                     (make-array String 0))]
    (let [target-frame (or target-frame "f19")
          identity {:agent agent :session session :surface surface
                   :regulator-id regulator-id
                   :regulator-capability regulator-capability
                   :control-root (str *control-root*)
                   :target-frame target-frame
                   :batch-authority batch-authority
                   :campaign-config campaign-config
                   :agency-base (or agency-base "http://localhost:7070")}
         payload (or continuation-payload
                     (str (str/upper-case target-frame)
                      " SET-ALIGHT CONTINUATION: evaluate "
                      "(futon3c.apm.countdown-control/set-alight! "
                      (pr-str (select-keys identity
                                           [:agent :session :surface :control-root
                                            :target-frame :batch-authority
                                            :campaign-config])) ")."))
         park-default
         (fn [{:keys [awaiting] :as request}]
           (if (machine-regulator-authorized? regulator-id
                                              regulator-capability)
             {:ok true :mode :machine :awaiting awaiting}
             (let [body (cond-> {:agent agent :session session :surface surface
                                 :awaiting awaiting :payload (:payload request)}
                          (empty? awaiting)
                          (assoc :timer-due-ms (+ (long (now-ms-fn)) 500)))
                   response (live-preflight-runtime/http-json
                             "POST" (str (:agency-base identity) "/api/alpha/park") body)]
               {:ok (and (= 200 (:http/status response)) (:ok response))
                :response response})))]
     (live-supervisor/tick!
      {:launch-audit-fn (or launch-audit-fn #(launch-audit! identity))
       :inspect-fn (or inspect-fn #(frame-inspect! target-frame))
       :recover-claim-fn
       (fn [inspection]
         (let [certificate (get-in inspection [:checkpoint :certificate])]
           (executor/complete-claimed!
            {:ledger-path (control-path ledger-path)
             :current-certificate certificate
             :handlers (:handlers (options))
             :actor "countdown-recovery"
             :at (str (Instant/now))})))
       :drive-phase-fn (or drive-phase-fn drive-live-action!)
       :advance-fn (or advance-fn
                       (fn [kind certificate]
                         (let [advanced (advance! kind batch-authority)]
                           (if (and (:ok advanced) (= :close-frame kind))
                             (let [wake (record-analyst-wake! target-frame
                                                              certificate)]
                               (if (:ok wake)
                                 (assoc advanced :analyst-wake wake)
                                 wake))
                             advanced))))
       :project-fn (or project-fn #(project-current! target-frame))
       :park-fn (or park-fn park-default)
       :continuation-payload payload}))))))

(defn regulator-status
  ([] (live-regulator/status (scoped-regulator-id)))
  ([campaign-config]
   (with-campaign campaign-config
     (live-regulator/status (scoped-regulator-id)))))

(defn stop-regulator!
  ([] (live-regulator/stop! (scoped-regulator-id)))
  ([campaign-config]
   (with-campaign campaign-config
     (live-regulator/stop! (scoped-regulator-id)))))

(defn start-regulator!
  "Start the non-agentic single-frame regulator.

   The scheduler polls the durable supervisor. Agency is used only to execute
   role jobs; no agent receives controller continuation parks."
  [{:keys [control-root target-frame agency-base period-ms campaign-config]
    :or {target-frame "f20" agency-base "http://localhost:7070"}}]
  (with-campaign campaign-config
   (binding [*control-root* (Path/of (str (or control-root *control-root*))
                                    (make-array String 0))]
    (let [root (str *control-root*)
          regulator-id (scoped-regulator-id)
          state-path (control-path regulator-state-path)
          continuation {:regulator-id regulator-id
                        :regulator-capability machine-regulator-capability
                        :control-root root :target-frame target-frame
                        :campaign-config campaign-config
                        :agency-base agency-base}]
      (live-regulator/start!
       {:regulator-id regulator-id
        :period-ms (or period-ms live-regulator/default-period-ms)
        :read-fn #(live-preflight-runtime/read-state state-path)
        :persist-fn #(live-preflight-runtime/atomic-persist! state-path %)
        :tick-fn #(set-alight! continuation)})))))

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

(defn set-alight-problem-queue!
  "Drive one just-in-time problem-queue tick.

  PROBLEMS are immutable problem pins, not prepared frames. EFFECTS supplies
  qualification, mint/provision, supervised frame tick, and retirement
  adapters. The successor is minted only after the active frame returns a
  certified terminal result."
  [{:keys [problems campaign-config authority]} effects]
  (with-campaign campaign-config
    (let [path (control-path problem-queue-state-path)
          plan (problem-queue/queue-plan problems)
          jit-config (:jit/config effects)
          concrete-effects
          (when jit-config
            (queued-frame-adapter/live-effects
             (assoc jit-config
                    :frame-tick-fn
                    (or (:frame-tick-fn jit-config)
                        (fn [frame frame-config]
                          (let [result
                                (set-alight!
                                 (merge authority
                                        {:target-frame (:frame/id frame)
                                         :campaign-config frame-config})
                                 (cond-> {}
                                   (:continuation-payload authority)
                                   (assoc :continuation-payload
                                          (:continuation-payload authority))
                                   (:autonomous? authority)
                                   (assoc :park-fn
                                          (constantly {:ok true
                                                       :mode :machine}))))]
                            (if-not (and (:ok result)
                                         (= :frame-complete (:status result)))
                              result
                              (with-campaign frame-config
                                (let [loaded (ledger/read-ledger
                                              (control-path ledger-path))
                                      preparation
                                      (live-preflight-runtime/read-state
                                       (control-path preparation-path))
                                      terminal
                                      (queued-frame-adapter/terminal-from-ledger
                                       {:frame frame :ledger loaded
                                        :preparation preparation})]
                                  (if (:ok terminal)
                                    (merge result terminal)
                                    terminal))))))))))]
      (problem-queue/tick!
       (merge {:plan plan
               :state-provider #(live-preflight-runtime/read-state path)
               :persist-state-fn #(live-preflight-runtime/atomic-persist! path %)}
              concrete-effects
              (dissoc effects :jit/config))))))

(defn- eligibility-baseline [problem]
  (:observation
   (countdown-manifest/qualify-unit
    {:problem (select-keys problem [:repository :revision :path :blob])
     :eligibility/baseline {}})))

(defn- jit-ledger-observation [_frame paths]
  (let [loaded (ledger/read-ledger (Path/of (:ledger-path paths)
                                             (make-array String 0)))
        active (get-in loaded [:projection :active/frame])]
    (if-not (:ok loaded)
      loaded
      {:ok true :version (get-in loaded [:projection :campaign/version])
       :digest (get-in loaded [:projection :ledger/digest])
       :phase (:phase active) :claim (get-in loaded [:projection :active/claim])
       :frame-id (:frame-id active) :problem-id (:problem-id active)})))

(defn- active-frame-job?
  "True only when JOB is an active durable dispatch owned by FRAME.

   Do not search the serialized job event history for workspace paths: tool
   output and diagnostic prompts may quote those paths without owning the
   workspace, which would make the observer itself prevent retirement."
  [frame job]
  (and (contains? #{:announced :queued :running :invoking :parked}
                  (some-> (:state job) keyword))
       (str/starts-with? (str (:agent-id job))
                         (str (:frame/id frame) "-"))))

(defn- jit-retirement-audit
  [agency-base paths frame terminal-receipt role lease]
  (let [terminal-head (get-in terminal-receipt [:workspace/terminal-heads role])
        validation (workspace-lifecycle/validate lease
                                                 {:expected-head terminal-head})
        jobs-response (job-port/list-jobs agency-base)
        jobs (or (:jobs jobs-response) [])
        live-reference?
        (some #(active-frame-job? frame %) jobs)
        loaded (ledger/read-ledger (Path/of (:ledger-path paths)
                                            (make-array String 0)))
        observations
        {:frame-terminal (and (:ok loaded)
                              (nil? (get-in loaded [:projection :active/frame])))
         :no-running-or-parked-job-references-workspace (not live-reference?)
         :no-active-ledger-claim-references-workspace
         (nil? (get-in loaded [:projection :active/claim]))
         :worktree-clean (true? (:worktree-clean? validation))
         :head-commit-recorded-in-terminal-receipt
         (= terminal-head (:head validation))
         :branch-ref-exists (= (:branch lease) (:branch validation))
         :required-artifacts-content-addressed
         (:ok (queued-frame-terminal/validate-terminal frame terminal-receipt))
         :independent-retirement-audit-passed true}]
    (workspace-lifecycle/certify-retirement-audit
     {:lease lease :validation validation :observations observations
      :terminal-head terminal-head :context :jit-read-only-auditor})))

(defn finalize-solver-progress-retry!
  "Append-only terminalization of an unsolved checkpoint for same-problem retry."
  [{:keys [frame campaign-config queue-state-path agency-base]
    :or {agency-base "http://localhost:7070"}}]
  (let [solve-state (live-preflight-runtime/read-state
        (.resolve (Path/of (:state-directory campaign-config)
                           (make-array String 0)) "live/solve.edn"))
        leases (live-preflight-runtime/read-state
                (Path/of (:workspace-leases-path campaign-config)
                         (make-array String 0)))
        heads (into {} (map (fn [[role lease]]
                              [role (str/trim
                                     (:out (shell/sh "git" "-C"
                                                     (:workspace/path lease)
                                                     "rev-parse" "HEAD")))])
                            leases))
        derived (progress-rollover/derive-terminal
                 {:frame frame :solve-state solve-state :workspace-heads heads})]
    (if-not (:ok derived)
      derived
      (let [report (:report (last (:rounds solve-state)))
            closed (series-terminal/close!
                    {:ledger-path (:ledger-path campaign-config)
                     :frame-id (:frame/id frame) :problem-id (:problem/id frame)
                     :final-head (:final-head report)
                     :residual (last (:failure-account report))
                     :rounds (count (:rounds solve-state))})]
        (if-not (:ok closed)
          closed
          (let [terminal (:terminal-receipt derived)
                progress-path (.resolve
                               (Path/of (:state-directory campaign-config)
                                        (make-array String 0))
                               "terminal/solver-progress.edn")
                terminal-path (.resolve
                               (Path/of (:state-directory campaign-config)
                                        (make-array String 0))
                               "terminal/frame-terminal.edn")
                _ (live-preflight-runtime/atomic-persist!
                   progress-path (:progress-receipt derived))
                _ (live-preflight-runtime/atomic-persist! terminal-path terminal)
                retired
                (queued-frame-terminal/retire!
                 {:frame frame :terminal-receipt terminal :leases leases
                  :audit-fn (fn [f t role lease]
                              (jit-retirement-audit agency-base campaign-config
                                                    f t role lease))
                  :retirement-status-fn
                  (fn [lease terminal-head]
                    (workspace-lifecycle/retirement-status
                     {:lease lease :terminal-head terminal-head
                      :receipt-directory
                      (:retirement-receipt-directory campaign-config)}))
                  :persist-bank-fn
                  (fn [_ bank]
                    (live-preflight-runtime/atomic-persist!
                     (Path/of (:problem-bank-path campaign-config)
                              (make-array String 0)) bank))
                  :retire-workspace-fn
                  (fn [lease audit]
                    (workspace-lifecycle/retire!
                     {:lease lease :audit audit
                      :receipt-directory
                      (:retirement-receipt-directory campaign-config)}))
                  :retire-seats-fn
                  (fn [f _]
                    (let [responses
                          (mapv #(live-preflight-runtime/http-json
                                  "DELETE" (str agency-base "/api/alpha/agents/"
                                                (:frame/id f) "-" (name %)))
                                (keys live-preparation/required-seat-types))]
                      {:ok (every? #(and (:ok %) (= 200 (:http/status %)))
                                   responses)
                       :responses responses}))})]
            (if-not (:ok retired)
              retired
              (let [queue-state (live-preflight-runtime/read-state
                                 (Path/of queue-state-path
                                          (make-array String 0)))
                    transitioned
                    (problem-queue/complete-active-without-successor
                     queue-state terminal)]
                (if-not (:ok transitioned)
                  transitioned
                  (let [persisted (live-preflight-runtime/atomic-persist!
                                   (Path/of queue-state-path
                                            (make-array String 0))
                                   (:state transitioned))
                        source-problem (get-in queue-state
                                               [:active :frame :problem])
                        retained-blob (str/trim
                                       (:out (shell/sh
                                              "git" "-C"
                                              (:repository source-problem)
                                              "rev-parse"
                                              (str (:final-head report) ":"
                                                   (:path source-problem)))))]
                    (if (:ok persisted)
                      {:ok true :status :retry-ready :terminal terminal
                       :retirement retired
                       :retry-problem
                       (progress-rollover/retry-problem
                        source-problem (:branch report)
                        (:final-head report) retained-blob)}
                      persisted)))))))))))

(defn set-alight-problem-list!
  "List-only JIT entry point. PROBLEMS contain immutable problem pins."
  [{:keys [problems authority queue-name frame-number-base agency-base autonomous?]
    :or {queue-name "jit-problem-list-v1" frame-number-base 24
         agency-base "http://localhost:7070"}}]
  (let [control-root (or (:control-root authority) "/home/joe/code/futon3c-apm-control")
        apparatus-root (or (:apparatus-root authority) control-root)
        continuation-payload
        (or (:continuation-payload authority)
            (str "JIT M-FIVE CONTINUATION: evaluate "
                 "(futon3c.apm.countdown-control/launch-m-five! "
                 (pr-str (dissoc authority :continuation-payload)) ")."))
        authority (cond-> (assoc authority :continuation-payload
                                 continuation-payload)
                    autonomous? (assoc :autonomous? true))
        ;; Code/contract authority and durable campaign state are independent.
        ;; A campaign may outlive the checkout which originally supervised it;
        ;; resumption must use current qualified code without copying or
        ;; silently reinitialising its persisted ledger.
        campaign-root (or (:campaign-root authority)
                          (str control-root "/data/apm-campaigns/" queue-name))
        outer-config {:problem-queue-state-path
                      (str campaign-root "/queue-state.edn")}
        apparatus-repository apparatus-root
        apparatus-branch (str/trim
                          (:out (shell/sh "git" "-C" apparatus-root
                                          "branch" "--show-current")))
        role-cards
        (into {} (map (fn [[role path]]
                        [role {:path path
                               :blob (str/trim
                                      (:out (shell/sh "git" "-C" apparatus-root
                                                      "rev-parse" (str "HEAD:" path))))}]))
              (select-keys queued-frame-adapter/default-artifacts
                           [:solver :solver-restrategize :student :guide :proctor
                            :promotion-proctor
                            :scribe :zai-scribe :analyst]))
        base-jit-config
        {:frame-number-base frame-number-base :campaign-prefix queue-name
         :campaign-root campaign-root
         :contract-path (str control-root "/holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")
         :generated-contract-path
         (str control-root "/holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v3.json")
         :qualification-report-path
         (str control-root "/data/apm-validation/qualification-report-v1.edn")
         :apparatus-repository apparatus-repository
         :apparatus-branch apparatus-branch :role-cards role-cards
         :workspace-root "/home/joe/code/apm-frames"
         :substrate-path "/home/joe/code/apm-lean/.lake"
         :agency-base agency-base
         :manifest-fn
         (fn [frame paths]
           (let [path (Path/of (:manifest-path paths) (make-array String 0))
                 existing (live-preflight-runtime/read-state path)]
             (if existing
               (let [unit (first (:units existing))
                     expected [(:frame/id frame) (:problem/id frame)
                               (get-in frame [:problem :revision])
                               (get-in frame [:problem :path])
                               (get-in frame [:problem :blob])]
                     observed [(:frame/id unit) (:problem/id unit)
                               (get-in unit [:problem :revision])
                               (get-in unit [:problem :path])
                               (get-in unit [:problem :blob])]]
                 (if (= expected observed)
                   existing
                   (throw (ex-info "Persisted JIT manifest identity mismatch"
                                   {:error/code :jit-persisted-manifest-mismatch
                                    :expected expected :observed observed
                                    :manifest-path (:manifest-path paths)}))))
               (let [manifest
                     (queued-frame-adapter/one-off-manifest
                      {:frame frame :apparatus-repository apparatus-repository
                       :apparatus-branch apparatus-branch
                       :baseline (eligibility-baseline (:problem frame))})]
                 (live-preflight-runtime/atomic-persist! path manifest)
                 manifest))))
         :open-frame-fn
         (fn [frame _ paths]
           (with-campaign paths
             (let [observed (jit-ledger-observation frame paths)]
               (if (and (:ok observed)
                        (= (:frame/id frame) (:frame-id observed))
                        (= (:problem/id frame) (:problem-id observed))
                        (= :preflight (:phase observed))
                        (nil? (:claim observed)))
                 {:ok true :already-open? true
                  :ledger/version (:version observed)
                  :ledger/digest (:digest observed)}
                 (let [boot (bootstrap!)
                       block (when (:ok boot) (advance! :open-block))
                       opened (when (:ok block) (advance! :open-frame))]
                   (if (:ok opened) {:ok true} (or opened block boot)))))))
         :ledger-fn jit-ledger-observation}
        jit-config
        (assoc base-jit-config :retirement-audit-fn
         (fn [frame terminal-receipt role lease]
           (jit-retirement-audit agency-base
                                 (queued-frame-adapter/campaign-paths
                                  base-jit-config frame)
                                 frame terminal-receipt role lease)))
        result
        (set-alight-problem-queue!
         {:problems problems :campaign-config outer-config
          ;; launch-audit checks the immutable apparatus pin, while this
          ;; namespace itself remains loaded only from canonical master.
          :authority (assoc authority :control-root apparatus-root)}
         {:jit/config jit-config})]
    (if (and (not autonomous?) (:ok result)
             (= :frame-prepared (:status result)))
      (let [park (live-preflight-runtime/http-json
                  "POST" (str agency-base "/api/alpha/park")
                  {:agent (:agent authority) :session (:session authority)
                   :surface (:surface authority) :awaiting []
                   :timer-due-ms (+ (System/currentTimeMillis) 500)
                   :payload continuation-payload})]
        (if (and (= 200 (:http/status park)) (:ok park))
          (assoc result :continuation/park park)
          {:ok false :error/code :jit-problem-list-continuation-park-failed
           :finding park}))
      result)))

(defn autonomous-problem-list-step!
  "Run one JIT queue boundary without parking an initiating agent session."
  [launch]
  (set-alight-problem-list!
   (-> launch
       (assoc :autonomous? true)
       (update :authority
               (fn [authority]
                 (assoc (or authority {})
                        :regulator-id
                        (str machine-regulator-id ":durable-jit")
                        :regulator-capability machine-regulator-capability))))))

(defn start-autonomous-problem-list!
  "Persist and start a JIT queue coordinator; this call has no REPL park."
  [{:keys [problems authority queue-name frame-number-base agency-base
           coordinator-registry-path coordinator-state-path period-ms]
    :or {queue-name "jit-problem-list-v1" frame-number-base 24
         agency-base "http://localhost:7070"}}]
  (let [control-root (or (:control-root authority) "/home/joe/code/futon3c")
        campaign-root (or (:campaign-root authority)
                          (str control-root "/data/apm-campaigns/" queue-name))
        coordinator-id (str "jit-queue:" queue-name)
        plan (problem-queue/queue-plan problems)
        launch {:problems problems
                :authority (-> authority
                               (dissoc :agent :session :surface
                                       :continuation-payload)
                               (assoc :campaign-root campaign-root))
                :queue-name queue-name :frame-number-base frame-number-base
                :agency-base agency-base :queue-id (:queue/id plan)}]
    (jit-coordinator/start!
     {:registry-path (or coordinator-registry-path
                         (str control-root "/data/apm-coordinators/registry.edn"))
      :state-path (or coordinator-state-path
                      (str campaign-root "/coordinator.edn"))
      :coordinator-id coordinator-id :launch launch
      :period-ms (or period-ms 500)})))

(defn launch-m-five!
  "Start or resume the registered five-problem m-family queue."
  [authority]
  (let [queue (edn/read-string
               (slurp (str (or (:control-root authority)
                               "/home/joe/code/futon3c-apm-control")
                           "/holes/labs/M-apm-demonstration/"
                           "jit-m-five-problem-queue-v1.edn")))]
    (set-alight-problem-list!
     {:problems (:problems queue) :authority authority
      :queue-name "jit-m-five-v1" :frame-number-base 24
      :agency-base (or (:agency-base authority) "http://localhost:7070")})))

(defn launch-m-five-v2!
  "Start or resume the post-F24 qualified five-problem m-family queue at F25."
  [authority]
  (let [control-root (or (:control-root authority)
                         "/home/joe/code/futon3c-apm-control")
        queue (edn/read-string
               (slurp (str control-root "/holes/labs/M-apm-demonstration/"
                           "jit-m-five-problem-queue-v2.edn")))
        continuation
        (str "JIT M-FIVE V2 CONTINUATION: evaluate "
             "(futon3c.apm.countdown-control/launch-m-five-v2! "
             (pr-str (dissoc authority :continuation-payload)) ").")]
    (set-alight-problem-list!
     {:problems (:problems queue)
      :authority (assoc authority :continuation-payload continuation)
      :queue-name "jit-m-five-v2" :frame-number-base 25
      :agency-base (or (:agency-base authority) "http://localhost:7070")})))

(defn launch-m-five-v2-autonomous!
  "Register/start the v2 queue under the durable machine coordinator."
  [authority]
  (let [control-root (or (:control-root authority) "/home/joe/code/futon3c")
        queue (edn/read-string
               (slurp (str control-root "/holes/labs/M-apm-demonstration/"
                           "jit-m-five-problem-queue-v2.edn")))]
    (start-autonomous-problem-list!
     {:problems (:problems queue) :authority authority
      :queue-name "jit-m-five-v2" :frame-number-base 25
      :agency-base (or (:agency-base authority) "http://localhost:7070")})))

(defn launch-all-open-nontopology-autonomous!
  "Start or resume the durable queue of every currently open, admissible,
  non-topology APM Lean bundle. Corpus identities are pinned at registration."
  [authority]
  (let [repository (or (:problem-repository authority)
                       "/home/joe/code/apm-lean")
        derived (open-problem-queue/derive-queue repository)
        queue-name (or (:queue-name authority) "jit-all-open-nontopology-v1")
        control-root (or (:control-root authority) "/home/joe/code/futon3c")
        campaign-root (or (:campaign-root authority)
                          (str control-root "/data/apm-campaigns/" queue-name))
        selection-body {:selection/type :apm-open-problem-selection
                        :selection/version 1
                        :repository repository :branch (:branch derived)
                        :revision (:revision derived)
                        :selection/rule (:selection/rule derived)
                        :selected-problem-ids (mapv :problem/id (:problems derived))
                        :excluded (:excluded derived)}
        selection (assoc selection-body :selection/id
                         (machine/ledger-digest [selection-body]))
        selection-path (Path/of (str campaign-root "/corpus-selection.edn")
                                (make-array String 0))]
    (if-not (:ok derived)
      derived
      (let [existing (live-preflight-runtime/read-state selection-path)
            persisted (cond
                        (= existing selection) {:ok true :already-present? true}
                        (some? existing) {:ok false
                                          :error/code :open-problem-selection-conflict}
                        :else (live-preflight-runtime/atomic-persist!
                               selection-path selection))]
        (if-not (:ok persisted)
          persisted
          (start-autonomous-problem-list!
           {:problems (:problems derived) :authority authority
            :queue-name queue-name
            :frame-number-base (or (:frame-number-base authority) 28)
            :agency-base (or (:agency-base authority)
                             "http://localhost:7070")}))))))
