(ns futon3c.apm.queued-frame-adapter
  "Ordering boundary for a just-in-time one-off frame.

  Registration/open establishes the authoritative :preflight ledger state;
  only then may workspaces and seats be provisioned and certified."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-qualification :as campaign-qualification]
            [futon3c.apm.live-launch-preparation :as live-preparation]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.qualification :as qualification]
            [futon3c.apm.queued-frame-terminal :as terminal]
            [futon3c.apm.workspace-lifecycle :as workspace])
  (:import [java.nio.file Files LinkOption Path]))

(declare mint qualify open-and-prepare!)

(defn- awaiting-claude-decision [park]
  (assoc park
         :decision/owner :claude-supervisor
         :decision/status :awaiting-decision
         :decision/bell-required true))

(defn solver-human-intervention-park
  "Convert a durably exhausted Solver checkpoint into a re-enterable frame
  park.  This does not certify the proof, retire its workspace, or decide
  whether Student should run later."
  [{:keys [frame ledger solver-state-path result]}]
  (let [state (:state result)
        completed (last (:rounds state))
        report (:report completed)
        receipt (last (keep #(get-in % [:event/body :certificate])
                            (:events ledger)))
        residual (last (:failure-account report))
        park (awaiting-claude-decision
              {:state/type :solver-human-intervention-frame-park
               :frame/id (:frame/id frame)
               :problem/id (:problem/id frame)
               :solver/rounds-completed (count (:rounds state))
               :solver/final-head (:final-head report)
               :solver/branch (:branch report)
               :solver/state-path solver-state-path
               :last-valid-receipt/id (or (:receipt/id receipt)
                                          (:certificate/id receipt))
               :residual residual
               :student/decision :claude-required})]
    (if (and (= :solver-human-intervention-required (:error/code result))
             (= :solver-human-intervention-required (:state/type state))
             (every? #(and (string? %) (not (str/blank? %)))
                     ((juxt :frame/id :problem/id :solver/final-head
                            :solver/state-path :last-valid-receipt/id
                            :residual) park))
             (pos-int? (:solver/rounds-completed park)))
      {:ok true :status :frame-parked :frame/park park}
      result)))

(defn scribe-reduce-apparatus-park
  "Park a frame whose completed mining dispatch cannot produce a valid
  deposit.  All prior phase receipts remain authoritative and untouched."
  [{:keys [frame ledger promotion-state-path result]}]
  (let [receipt (last (keep #(get-in % [:event/body :certificate])
                            (:events ledger)))
        park (awaiting-claude-decision
              {:state/type :scribe-reduce-apparatus-frame-park
               :frame/id (:frame/id frame)
               :problem/id (:problem/id frame)
               :phase :scribe-reduce
               :promotion/state-path promotion-state-path
               :last-valid-receipt/id (or (:receipt/id receipt)
                                          (:certificate/id receipt))
               :error/code (:error/code result)
               :deposit/attempts (:attempts result)
               :deposit/findings (:findings result)
               :residual (pr-str (:findings result))})]
    (if (and (= :promotion-deposit-retries-exhausted (:error/code result))
             (every? #(and (string? %) (not (str/blank? %)))
                     ((juxt :frame/id :problem/id :promotion/state-path
                            :last-valid-receipt/id :residual) park))
             (pos-int? (:deposit/attempts park))
             (seq (:deposit/findings park)))
      {:ok true :status :frame-parked :frame/park park}
      result)))

(defn promotion-apparatus-park
  "Park a frame after bounded promotion projection/publication repair is
  exhausted. The persisted merit judgement and last valid phase receipt remain
  re-enterable; this park does not disposition the affected candidate."
  [{:keys [frame ledger result]}]
  (let [receipt (last (keep #(get-in % [:event/body :certificate])
                            (:events ledger)))
        state (:state result)
        park (awaiting-claude-decision
              {:state/type :promotion-apparatus-frame-park
               :frame/id (:frame/id frame)
               :problem/id (:problem/id frame)
               :phase :promotion
               :promotion/state-path (:promotion/state-path result)
               :last-valid-receipt/id (or (:receipt/id receipt)
                                          (:certificate/id receipt))
               :error/code (:error/code result)
               :repair/kind (:repair/kind result)
               :repair/attempts (:repair/attempts result)
               :promotion/findings (:findings result)
               :persisted-review-result (:persisted-review-result state)
               :residual (pr-str (:findings result))})]
    (if (and (= :promotion-apparatus-repair-exhausted (:error/code result))
             (every? #(and (string? %) (not (str/blank? %)))
                     ((juxt :frame/id :problem/id :promotion/state-path
                            :last-valid-receipt/id :residual) park))
             (keyword? (:repair/kind park))
             (pos-int? (:repair/attempts park))
             (seq (:promotion/findings park)))
      {:ok true :status :frame-parked :frame/park park}
      result)))

(def default-artifacts
  {:cycle-contract "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"
   :typed-completion
   "holes/labs/M-apm-demonstration/role-cards/typed-completion-v1.md"
   :solver "holes/labs/M-apm-demonstration/role-cards/codex-solver-v5.md"
   :solver-restrategize
   "holes/labs/M-apm-demonstration/role-cards/solver-restrategize-v1.md"
   :student "holes/labs/M-apm-demonstration/role-cards/zai-student-v2.md"
   :guide "holes/labs/M-apm-demonstration/role-cards/claude-guide-v2.3.md"
   :scribe "holes/labs/M-apm-demonstration/role-cards/codex-scribe-v2.md"
   :zai-scribe "holes/labs/M-apm-demonstration/role-cards/zai-scribe-v2.md"
   :proctor "holes/labs/M-apm-demonstration/role-cards/proctor.md"
   :promotion-proctor
   "holes/labs/M-apm-demonstration/role-cards/promotion-proctor-v2.md"
   :analyst "holes/labs/M-apm-demonstration/role-cards/analyst-v1.md"})

(defn- git-out [repository & args]
  (let [result (apply shell/sh (concat ["git" "-C" repository] args))]
    (when (zero? (:exit result)) (str/trim (:out result)))))

(defn one-off-manifest
  "Generate a content-addressed one-off manifest from immutable Git objects."
  [{:keys [frame apparatus-repository apparatus-branch artifacts baseline]}]
  (let [revision (git-out apparatus-repository "rev-parse"
                          (str "refs/heads/" apparatus-branch))
        artifact-pins
        (into {} (map (fn [[role path]]
                        [role {:path path
                               :blob (git-out apparatus-repository "rev-parse"
                                              (str revision ":" path))}]))
              (or artifacts default-artifacts))
        apparatus-body {:repository apparatus-repository :branch apparatus-branch
                        :revision revision :artifacts artifact-pins}
        apparatus (assoc apparatus-body :pin/id
                         (machine/ledger-digest [apparatus-body]))
        problem (:problem frame)
        unit {:frame/id (:frame/id frame) :ordinal 1 :arm :treatment
              :problem/id (:problem/id frame)
              :classification/value :non-topology
              :classification/source :operator-reviewed-statement
              :classification/evidence "Queue admission: operator-selected non-topology m-family problem."
              :eligibility/baseline baseline
              :apparatus/pin-id (:pin/id apparatus)
              :problem {:repository (:repository problem)
                        :branch (or (:base-branch problem) "master")
                        :revision (:revision problem) :path (:path problem)
                        :blob (:blob problem)}}
        body {:manifest/version 2 :manifest/scope :one-off
              :campaign/id (:campaign/id frame)
              :block/id (str (:frame/id frame) "-one-off")
              :apparatus apparatus :units [unit]}]
    (if (and revision (every? (comp string? :blob val) artifact-pins))
      (assoc body :manifest/id (machine/ledger-digest [body]))
      {:error/code :queued-frame-apparatus-pin-unavailable})))

(defn campaign-paths
  "Derive every mutable campaign path from the minted campaign identity."
  [{:keys [campaign-root problem-buffer-prefix contract-path
           generated-contract-path qualification-report-path]} frame]
  (let [root (.resolve (Path/of (str campaign-root) (make-array String 0))
                       (:campaign/id frame))]
    {:state-directory (str root)
     :manifest-path (str (.resolve root "manifest.edn"))
     :ledger-path (str (.resolve root "ledger.edn"))
     :preflight-state-path (str (.resolve root "live/preflight.edn"))
     :workspace-leases-path (str (.resolve root "live/workspace-leases.edn"))
     :regulator-state-path (str (.resolve root "live/regulator.edn"))
     :analyst-state-path (str (.resolve root "analyst/state.edn"))
     :batch-cursor-path (str (.resolve root "live/batch-cursor.edn"))
     :problem-queue-state-path (str (.resolve root "live/problem-queue.edn"))
     ;; Outer queue order is the authority for campaign-cumulative memory.
     ;; Keep it distinct from this one-off frame's internal problem queue.
     :campaign-queue-state-path
     (str (.resolve (Path/of (str campaign-root) (make-array String 0))
                    "queue-state.edn"))
     :certificate-directory (str (.resolve root "certificates"))
     :projection-directory (str (.resolve root "projection"))
     :problem-buffer-path (str (.resolve root "problem-buffer.md"))
     :preparation-path (str (.resolve root "preparation.edn"))
     :frame-terminal-path (str (.resolve root "terminal/frame-terminal.edn"))
     :problem-bank-path (str (.resolve root "terminal/problem-bank.edn"))
     :retirement-receipt-directory (str (.resolve root "terminal/workspaces"))
     :contract-path contract-path
     :generated-contract-path generated-contract-path
     :qualification-report-path qualification-report-path
     ;; Durable files remain frame-scoped. The operator-facing Emacs buffer is
     ;; a singleton which follows the queue across frame transitions.
     :problem-buffer-name (or problem-buffer-prefix "*problem*")}))

(defn qualify-current
  "Qualify a mint against the exact generated artifact and passing report."
  [{:keys [frame generated-contract-path qualification-report-path]}]
  (try
    (let [report (edn/read-string (slurp qualification-report-path))
          report-check (qualification/validate-report report generated-contract-path)
          artifact-digest (qualification/file-digest generated-contract-path)
          report-digest (machine/ledger-digest [(slurp qualification-report-path)])
          shape (qualify {:frame frame
                          :generated-contract-digest artifact-digest
                          :qualification-digest report-digest})]
      (if (and (:ok shape) (:ok report-check))
        {:ok true :frame frame :qualification report-check
         :generated-contract-digest artifact-digest
         :qualification-report-digest report-digest}
        {:ok false :error/code :queued-frame-qualification-invalid
         :shape shape :report report-check}))
    (catch Throwable t
      {:ok false :error/code :queued-frame-qualification-unreadable
       :finding {:message (.getMessage t)}})))

(defn terminal-from-ledger
  "Derive queue terminal evidence from the validated ledger and preparation."
  [{:keys [frame ledger preparation prior-terminal]}]
  (let [certificates (keep #(get-in % [:event/body :certificate]) (:events ledger))
        solve (some #(when (= :frame-solve (:receipt/type %)) %) certificates)
        verify (some #(when (= :frame-verify (:receipt/type %)) %) certificates)
        close (some #(when (= :frame-close (:receipt/type %)) %) certificates)
        void (some #(when (= :frame-void (:certificate/type %)) %) certificates)
        observation-missing? (some #(= :student-observation-missing
                                       (:receipt/type %)) certificates)
        raw-result (or (:receipt/result close) (when void :void))
        frame-result (cond
                       (contains? #{:closed "closed"} raw-result) :closed
                       (contains? #{:partial "partial"} raw-result) :partial
                       (contains? #{:void "void"} raw-result) :void
                       :else nil)
        prior-terminal-valid? (:ok (terminal/validate-terminal frame prior-terminal))
        workspace-head
        (fn [role]
          (let [workspace (get-in preparation [:workspaces role])]
            (or (:terminal-head workspace)
                (when-let [path (:workspace/path workspace)]
                  (let [result (shell/sh "git" "-C" path "rev-parse" "HEAD")]
                    (when (zero? (:exit result)) (str/trim (:out result)))))
                (when prior-terminal-valid?
                  (get-in prior-terminal [:workspace/terminal-heads role])))))
        solver-workspace (get-in preparation [:workspaces :solver])
        heads {:solver (workspace-head :solver) :student (workspace-head :student)}
        body {:receipt/type :frame-terminal
              :frame/id (:frame/id frame) :problem/id (:problem/id frame)
              :frame/result frame-result
              :problem/outcome (if void :invalid
                                 (if (and (= 0 (get-in solve [:receipt/lean
                                                             :sorry-warnings]))
                                        (true? (:receipt/mathematical-sound? verify)))
                                   :solved :partial))
              :learning/outcome (if void :skipped
                                  (or (:receipt/learning-outcome close)
                                    (when observation-missing? :partially-observed)
                                    :observed))
              :verify-receipt/id (:receipt/id verify)
              :solver {:branch (:branch solver-workspace)
                       :head (or (:receipt/final-head solve) (:solver heads))}
              :void/certificate-id (:certificate/id void)
              :void/classification (:classification void)
              :workspace/terminal-heads heads}
        receipt (assoc body :receipt/id (machine/ledger-digest [body]))
        checked (terminal/validate-terminal frame receipt)]
    (if (:ok checked)
      {:ok true :frame/result frame-result :terminal-receipt receipt}
      {:ok false :error/code :queued-frame-terminal-derivation-failed
       :terminal-check checked
       :missing (cond-> [] (nil? solve) (conj :solve-receipt)
                        (nil? verify) (conj :verify-receipt)
                        (and (nil? close) (nil? void)) (conj :close-or-void))})))

(defn- roster-seats [response frame-id]
  (let [ids (into {} (map (fn [role] [role (str frame-id "-" (name role))])
                           (keys live-preparation/required-seat-types)))
        agents (or (:agents response) (get response "agents"))
        policies (campaign-qualification/seat-configs-from-roster response ids)]
    (into {}
          (map (fn [[role agent-id]]
                 (let [agent (or (get agents agent-id)
                                 (get agents (keyword agent-id))
                                 (get agents (str agent-id)))]
                   [role {:agent-id agent-id
                          :type (some-> (or (:type agent) (get agent "type")) keyword)
                          :frame-id frame-id
                          :invoke-ready? (true? (or (:invoke-ready? agent)
                                                   (get agent "invoke-ready?")))
                          :effective-timeouts (get policies role)}]))
          ids))))

(defn prepare-live!
  "Provision and certify one already-open frame using the production adapters.

  HTTP-FN remains injectable so qualification never dispatches a live role."
  [{:keys [frame ledger manifest role-cards workspace-root substrate-path
           leases agency-base http-fn provision-fn validate-workspace-fn
           bootstrap-workspace-fn persist-lease-fn]
    :or {agency-base "http://localhost:7070" http-fn runtime/http-json
         provision-fn workspace/provision! validate-workspace-fn workspace/validate
         bootstrap-workspace-fn
         (fn [lease]
           (let [result (shell/sh "lake" "build" "ConstructionTargets"
                                  :dir (:workspace/path lease))]
             (if (zero? (:exit result))
               {:ok true}
               {:ok false :error/code :workspace-bootstrap-failed
                :role (:role lease) :finding result})))}}]
  (let [unit (assoc (:problem frame) :frame/id (:frame/id frame)
                    :problem/id (:problem/id frame)
                    :problem (:problem frame))
        exists? (fn [_ role]
                  (let [lease (get leases role)]
                    (and lease
                         (Files/exists (Path/of (:workspace/path lease)
                                               (make-array String 0))
                                       (make-array LinkOption 0)))))
        minted-response (atom nil)
        roster-response (atom nil)
        provisioned-leases (atom {})
        result
        (live-preparation/prepare!
         {:unit unit :ledger ledger :role-cards role-cards :leases (or leases {})
          :workspace-exists? exists?
          :provision-fn
          (fn [unit role]
            (let [provisioned (provision-fn {:unit unit :role role
                                             :workspace-root workspace-root
                                             :substrate-path substrate-path})]
              (when (:ok provisioned)
                (let [lease (:lease provisioned)
                      bootstrapped (bootstrap-workspace-fn lease)
                      persisted (when (and (:ok bootstrapped) persist-lease-fn)
                                  (persist-lease-fn role lease))]
                  (when-not (:ok bootstrapped)
                    (throw (ex-info "Workspace bootstrap failed"
                                    {:role role :bootstrap bootstrapped})))
                  (if (and persist-lease-fn (not (:ok persisted)))
                    (throw (ex-info "Workspace lease persistence failed"
                                    {:role role :persistence persisted}))
                    (swap! provisioned-leases assoc role lease))))
              provisioned))
          :validate-workspace-fn validate-workspace-fn
          :mint-fn
          (fn [frame-id seat-types _timeouts]
            (let [cast (into {} (map (fn [[role type]]
                                       [(name role) {:type (name type)}]) seat-types))
                  response (http-fn "POST" (str agency-base
                                                "/api/alpha/frames/mint-seats")
                                    {:frame-id frame-id :cast cast})]
              (reset! minted-response response)
              {:ok (and (= 200 (:http/status response)) (:ok response))}))
          :roster-fn
          (fn [frame-id]
            (let [response (http-fn "GET" (str agency-base "/api/alpha/agents"))]
              (reset! roster-response response)
              (roster-seats response frame-id)))})]
    (if-not (:ok result)
      result
      (let [workspaces
            (into {} (map (fn [[role workspace-result]]
                            [role (:lease workspace-result)]))
                  (for [role live-preparation/required-workspace-roles]
                    [role {:lease (or (get leases role)
                                      (get @provisioned-leases role))}]))]
        (if (some nil? (vals workspaces))
          {:ok false :error/code :queued-frame-workspace-lease-not-observable}
          (let [seats (roster-seats @roster-response (:frame/id frame))
                body {:preparation/version 2 :campaign/id (:campaign/id frame)
                      :frame/id (:frame/id frame) :problem/id (:problem/id frame)
                      :manifest/id (:manifest/id manifest)
                      :ledger (select-keys ledger [:version :digest :phase :claim])
                      :workspaces workspaces :seats seats
                      :seat-policy {:fresh-sessions? true :invoke-ready? true
                                    :turn-timeout-ms 3600000
                                    :student-turn-timeout-ms 1800000
                                    :zai-request-timeout-ms 300000}}]
            {:ok true :preparation
             (assoc body :preparation/id (machine/ledger-digest [body]))
             :launch-receipt (:receipt result)
             :seat-mint @minted-response}))))))

(defn live-effects
  "Build queue-supervisor effects for JIT preparation and supervised execution.

  OPEN-FRAME-FN and FRAME-TICK-FN are countdown-control boundaries.  They are
  explicit to avoid a namespace cycle; all resource effects below use the
  production lifecycle/Agency adapters."
  [{:keys [frame-number-base campaign-prefix generated-contract-path
           qualification-report-path manifest-fn ledger-fn
           role-cards workspace-root substrate-path agency-base http-fn
           open-frame-fn frame-tick-fn retire-frame-fn retirement-audit-fn
           persist-fn]
    :as config}]
  {:mint-frame-fn
   #(mint (assoc % :frame-number-base frame-number-base
                 :campaign-prefix campaign-prefix))
   :qualify-frame-fn
   #(qualify-current {:frame % :generated-contract-path generated-contract-path
                      :qualification-report-path qualification-report-path})
   :prepare-frame-fn
   (fn [frame]
     (let [paths (campaign-paths config frame)
           manifest (manifest-fn frame paths)
           lease-path (Path/of (:workspace-leases-path paths)
                               (make-array String 0))
           persisted-leases (or (runtime/read-state lease-path) {})
           lease-state (atom persisted-leases)
           result
           (open-and-prepare!
            {:frame frame
             :open-frame-fn #(open-frame-fn % manifest paths)
             :preparation-observation-fn #(ledger-fn % paths)
             :prepare-frame-fn
             (fn [opened-frame ledger]
               (prepare-live!
                {:frame opened-frame :ledger ledger :manifest manifest
                 :role-cards role-cards :workspace-root workspace-root
                 :substrate-path substrate-path :leases persisted-leases
                 :persist-lease-fn
                 (fn [role lease]
                   (let [next-state (swap! lease-state assoc role lease)]
                     ((or persist-fn runtime/atomic-persist!) lease-path
                      next-state)))
                 :agency-base agency-base :http-fn (or http-fn runtime/http-json)}))
             :persist-preparation-fn
             (fn [_ preparation]
               ((or persist-fn runtime/atomic-persist!)
                (Path/of (:preparation-path paths) (make-array String 0))
                preparation))})]
       (if (:ok result)
         (assoc result :campaign-config paths)
         result)))
   :frame-tick-fn
   (fn [frame]
     (frame-tick-fn frame (campaign-paths config frame)))
   :retire-frame-fn
   (or retire-frame-fn
       (fn [{:keys [frame terminal-receipt]}]
         (let [paths (campaign-paths config frame)
               leases (runtime/read-state
                       (Path/of (:workspace-leases-path paths)
                                (make-array String 0)))
               terminal-check (terminal/validate-terminal frame terminal-receipt)
               terminal-persisted
               (when (:ok terminal-check)
                 ((or persist-fn runtime/atomic-persist!)
                  (Path/of (:frame-terminal-path paths) (make-array String 0))
                  terminal-receipt))]
           (cond
             (not (:ok terminal-check)) terminal-check
             (not (:ok terminal-persisted))
             {:ok false :error/code :queued-frame-terminal-persistence-failed}
             :else
             (terminal/retire!
              {:frame frame :terminal-receipt terminal-receipt :leases leases
             :audit-fn retirement-audit-fn
             :retirement-status-fn
             (fn [lease terminal-head]
               (workspace/retirement-status
                {:lease lease :terminal-head terminal-head
                 :receipt-directory (:retirement-receipt-directory paths)}))
             :persist-bank-fn
             (fn [_ bank]
               ((or persist-fn runtime/atomic-persist!)
                (Path/of (:problem-bank-path paths) (make-array String 0)) bank))
             :retire-workspace-fn
             (fn [lease audit]
               (workspace/retire!
                {:lease lease :audit audit
                 :receipt-directory (:retirement-receipt-directory paths)}))
             :retire-seats-fn
             (fn [retired-frame _]
               (let [responses
                     (mapv (fn [role]
                             ((or http-fn runtime/http-json)
                              "DELETE" (str (or agency-base "http://localhost:7070")
                                            "/api/alpha/agents/"
                                            (:frame/id retired-frame) "-"
                                            (name role))))
                           (keys live-preparation/required-seat-types))]
                 {:ok (every? #(and (:ok %) (= 200 (:http/status %))) responses)
                  :responses responses}))})))))})

(defn mint
  [{:keys [problem ordinal queue/id frame-number-base campaign-prefix]}]
  (let [frame-id (str "f" (+ (or frame-number-base 1) ordinal))
        campaign-id (str (or campaign-prefix "apm-queued") "-" frame-id)
        body {:frame/id frame-id :problem/id (:problem/id problem)
              :problem problem :campaign/id campaign-id :queue/id id
              :ordinal ordinal}]
    {:ok true :frame (assoc body :frame/mint-id
                            (machine/ledger-digest [body]))}))

(defn valid-mint? [frame]
  (and (map? frame)
       (= (:frame/mint-id frame)
          (machine/ledger-digest [(dissoc frame :frame/mint-id)]))))

(defn qualify
  [{:keys [frame generated-contract-digest qualification-digest]}]
  (let [findings (cond-> []
                   (not (valid-mint? frame)) (conj :frame-mint-invalid)
                   (not (and (string? generated-contract-digest)
                             (re-matches #"[0-9a-f]{64}"
                                         generated-contract-digest)))
                   (conj :generated-contract-digest-invalid)
                   (not (and (string? qualification-digest)
                             (re-matches #"[0-9a-f]{64}"
                                         qualification-digest)))
                   (conj :qualification-digest-invalid))]
    (if (seq findings)
      {:ok false :error/code :queued-frame-qualification-invalid
       :findings findings}
      {:ok true :frame frame})))

(defn open-and-prepare!
  "Open the one-off ledger, observe exact preflight authority, then provision.

  OPEN-FRAME-FN owns registration/open effects. PREPARE-FRAME-FN owns
  workspace/seat effects and must return a content-addressed preparation."
  [{:keys [frame open-frame-fn preparation-observation-fn prepare-frame-fn
           persist-preparation-fn]}]
  (cond
    (not (valid-mint? frame))
    {:ok false :error/code :queued-frame-mint-invalid}
    (not (every? fn? [open-frame-fn preparation-observation-fn
                      prepare-frame-fn persist-preparation-fn]))
    {:ok false :error/code :queued-frame-provider-missing}
    :else
    (let [opened (open-frame-fn frame)]
      (if-not (:ok opened)
        opened
        (let [ledger (preparation-observation-fn frame)
              authoritative? (and (:ok ledger) (= 5 (:version ledger))
                                  (= :preflight (:phase ledger))
                                  (nil? (:claim ledger))
                                  (= (:frame/id frame) (:frame-id ledger))
                                  (= (:problem/id frame) (:problem-id ledger)))]
          (if-not authoritative?
            {:ok false :error/code :queued-frame-preparation-authority-invalid
             :finding ledger}
            (let [prepared (prepare-frame-fn frame ledger)]
              (if-not (:ok prepared)
                prepared
                (let [receipt (:preparation prepared)
                      addressed? (= (:preparation/id receipt)
                                    (machine/ledger-digest
                                     [(dissoc receipt :preparation/id)]))]
                  (if-not addressed?
                    {:ok false :error/code
                     :queued-frame-preparation-content-invalid}
                    (let [persisted (persist-preparation-fn frame receipt)]
                      (if (:ok persisted)
                        {:ok true :preparation/id (:preparation/id receipt)
                         :frame frame :ledger ledger}
                        {:ok false :error/code
                         :queued-frame-preparation-persistence-failed}))))))))))))
