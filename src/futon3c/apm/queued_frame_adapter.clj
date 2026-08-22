(ns futon3c.apm.queued-frame-adapter
  "Ordering boundary for a just-in-time one-off frame.

  Registration/open establishes the authoritative :preflight ledger state;
  only then may workspaces and seats be provisioned and certified."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-qualification :as campaign-qualification]
            [futon3c.apm.live-launch-preparation :as live-preparation]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.qualification :as qualification]
            [futon3c.apm.workspace-lifecycle :as workspace])
  (:import [java.nio.file Files LinkOption Path]))

(declare mint qualify open-and-prepare!)

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
     :certificate-directory (str (.resolve root "certificates"))
     :projection-directory (str (.resolve root "projection"))
     :problem-buffer-path (str (.resolve root "problem-buffer.md"))
     :preparation-path (str (.resolve root "preparation.edn"))
     :contract-path contract-path
     :generated-contract-path generated-contract-path
     :qualification-report-path qualification-report-path
     :problem-buffer-name (str (or problem-buffer-prefix "*problem: ")
                               (:frame/id frame) "-" (:problem/id frame) "*")}))

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
           persist-lease-fn]
    :or {agency-base "http://localhost:7070" http-fn runtime/http-json
         provision-fn workspace/provision! validate-workspace-fn workspace/validate}}]
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
                      persisted (when persist-lease-fn
                                  (persist-lease-fn role lease))]
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
           open-frame-fn frame-tick-fn retire-frame-fn persist-fn]
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
   :retire-frame-fn retire-frame-fn})

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
