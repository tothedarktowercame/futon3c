(ns futon3c.apm.library-lane-launch
  "Compose observed problem, workspace, seat, and receipt authority for one
  Codex-only library-lane frame. This boundary prepares; it never dispatches."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.library-lane-adapters :as adapters]
            [futon3c.apm.library-lane-runner :as runner]
            [futon3c.apm.live-launch-preparation :as preparation]))

(def phase-actions
  {:preflight {:timeouts
               {:request-ms (:request-timeout-ms preparation/required-timeouts)
                :turn-ms (:turn-timeout-ms preparation/required-timeouts)}}
   :solve {:timeouts {:turn-ms (:turn-timeout-ms preparation/required-timeouts)}}
   :verify {:timeouts {:turn-ms (:turn-timeout-ms preparation/required-timeouts)}}})

(defn- run-default [dir argv]
  (apply shell/sh (concat (map str argv) [:dir (str dir)])))

(defn- git-value [run-fn root & argv]
  (let [result (run-fn root (into ["git" "-C" (str root)] argv))]
    (when (zero? (:exit result)) (str/trim (:out result)))))

(defn observe-problem
  "Read one problem pin from its bundle and enclosing Git checkout.

  RUN-FN is injectable so callers can replace every process effect. No branch,
  revision, path, or blob is defaulted."
  [{:keys [corpus-root problem-id run-fn] :or {run-fn run-default}}]
  (let [status-path (io/file corpus-root "problems" problem-id "status.json")
        problem-path (str "problems/" problem-id "/lean/Main.lean")
        main-path (io/file corpus-root problem-path)]
    (cond
      (not (.isFile status-path))
      {:ok false :error/code :problem-bundle-missing
       :findings [{:finding :status-json-missing :path (str status-path)}]}

      (not (.isFile main-path))
      {:ok false :error/code :problem-bundle-unreadable
       :findings [{:finding :problem-main-missing :path problem-path}]}

      :else
      (try
        (let [status (json/parse-string (slurp status-path) true)
              repository (git-value run-fn corpus-root "rev-parse" "--show-toplevel")
              branch (git-value run-fn corpus-root "branch" "--show-current")
              revision (git-value run-fn corpus-root "rev-parse" "HEAD")
              blob (git-value run-fn corpus-root "rev-parse"
                              (str "HEAD:" problem-path))
              findings
              (cond-> []
                (not= problem-id (:problem_id status))
                (conj {:finding :problem-id-mismatch
                       :expected problem-id :observed (:problem_id status)})
                (not (every? #(and (string? %) (not (str/blank? %)))
                             [repository branch revision blob]))
                (conj {:finding :problem-git-pin-unreadable}))]
          (if (seq findings)
            {:ok false :error/code :problem-bundle-unreadable
             :findings findings}
            {:ok true
             :problem {:repository repository :branch branch
                       :revision revision :path problem-path :blob blob}}))
        (catch Throwable t
          {:ok false :error/code :problem-bundle-unreadable
           :findings [{:finding :status-json-unreadable
                       :message (.getMessage t)}]})))))

(defn- ledger [frame-id problem-id revision]
  (let [body {:version 5 :phase :preflight :claim nil
              :frame/id frame-id :problem/id problem-id
              :problem/revision revision}]
    (assoc body :digest (machine/ledger-digest [body]))))

(defn- actions [frame-id problem-id]
  (into {}
        (map (fn [[phase action]]
               [phase (assoc action :frame-id frame-id :problem-id problem-id)]))
        phase-actions))

(defn- state-paths [state-root frame-id]
  (into {}
        (map (fn [phase]
               [phase (str (io/file state-root frame-id
                                    (str (name phase) ".edn")))]))
        [:preflight :solve :verify]))

(defn- launch-findings
  [{:keys [corpus-root problem-id trunk-branch keying-target state-root
           agency-base occupied-frame-ids observe-problem-fn leases-fn
           workspace-exists? provision-fn validate-workspace-fn mint-fn
           roster-fn outcome-fn]}]
  (cond-> []
    (not (and (string? corpus-root) (not (str/blank? corpus-root))))
    (conj :corpus-root-missing)
    (not (and (string? problem-id) (not (str/blank? problem-id))))
    (conj :problem-id-missing)
    (not (and (string? trunk-branch) (not (str/blank? trunk-branch))))
    (conj :trunk-branch-missing)
    (not (and (string? keying-target) (not (str/blank? keying-target))))
    (conj :keying-target-missing)
    (not (and (string? state-root) (not (str/blank? state-root))))
    (conj :state-root-missing)
    (not (and (string? agency-base) (not (str/blank? agency-base))))
    (conj :agency-base-missing)
    (not (set? occupied-frame-ids)) (conj :occupied-frame-id-set-missing)
    (not (every? fn? [observe-problem-fn leases-fn workspace-exists?
                      provision-fn validate-workspace-fn mint-fn roster-fn
                      outcome-fn]))
    (conj :launch-effect-provider-missing)))

(defn launch!
  "Prepare and return the exact configuration consumed by both lane adapters.

  Provisioning, lease lookup, validation, seat minting/observation, problem
  observation, and outcome classification are all injected effects."
  [{:keys [problem-id trunk-branch keying-target state-root agency-base
           occupied-frame-ids observe-problem-fn leases-fn provision-fn
           roster-fn outcome-fn]
    :as options}]
  (let [findings (launch-findings options)]
    (if (seq findings)
      {:ok false :error/code :library-lane-launch-input-invalid
       :findings findings}
      (let [observed (observe-problem-fn
                      (select-keys options [:corpus-root :problem-id]))]
        (if-not (:ok observed)
          {:ok false :error/code :library-lane-problem-observation-failed
           :findings [observed]}
          (let [problem (:problem observed)
                ;; The frame id is content-addressed on [problem-id revision],
                ;; so a rerun of the SAME problem derives the SAME id. Its own
                ;; seats and workspace from a previous attempt are therefore in
                ;; the occupied set, and a naive check refuses the retry --
                ;; which would make launch! idempotent in fixtures and
                ;; single-shot in reality. Reuse is the intended behaviour; a
                ;; genuine cross-problem digest collision is not.
                ;;
                ;; Distinguish them by ownership: a persisted solver lease for
                ;; this exact unit proves the frame is ours. Only an occupied
                ;; id with NO such lease is a foreign collision.
                candidate (adapters/codex-frame-id problem-id (:revision problem)
                                                   #{})
                self-owned?
                (boolean
                 (when (:ok candidate)
                   (seq (or (leases-fn {:frame/id (:frame-id candidate)
                                        :problem/id problem-id
                                        :problem problem})
                            {}))))
                frame (if self-owned?
                        candidate
                        (adapters/codex-frame-id problem-id (:revision problem)
                                                 occupied-frame-ids))]
            (cond
              (not= trunk-branch (:branch problem))
              {:ok false :error/code :library-lane-trunk-mismatch
               :findings [{:finding :observed-trunk-mismatch
                           :expected trunk-branch :observed (:branch problem)}]}

              (not (:ok frame))
              {:ok false :error/code :library-lane-frame-id-refused
               :findings [frame]}

              :else
              (let [frame-id (:frame-id frame)
                    unit {:frame/id frame-id :problem/id problem-id
                          :problem problem}
                    existing-leases (or (leases-fn unit) {})
                    provisioned (atom {})
                    recording-provision
                    (fn [u role]
                      (let [result (provision-fn u role)]
                        (when (:ok result)
                          (swap! provisioned assoc role (:lease result)))
                        result))
                    prepared
                    (adapters/prepare-codex-only!
                     {:unit unit :ledger (ledger frame-id problem-id
                                                (:revision problem))
                      :role-cards {:solver runner/library-card
                                   :proctor runner/library-card}
                      :leases existing-leases
                      :workspace-exists? (:workspace-exists? options)
                      :provision-fn recording-provision
                      :validate-workspace-fn (:validate-workspace-fn options)
                      :mint-fn (:mint-fn options) :roster-fn roster-fn})]
                (if-not (:ok prepared)
                  {:ok false :error/code :library-lane-preparation-failed
                   :findings [prepared]}
                  (let [leases (merge existing-leases @provisioned)
                        workspace (get leases :solver)
                        seats (roster-fn frame-id)
                        config {:unit unit
                                :ledger (ledger frame-id problem-id
                                                (:revision problem))
                                :workspace workspace :seats seats
                                :actions (actions frame-id problem-id)
                                :state-paths (state-paths state-root frame-id)
                                :agency-base agency-base
                                :trunk-branch trunk-branch
                                :keying-target keying-target
                                :outcome-fn outcome-fn}]
                    (if-not (and (map? workspace)
                                 (= (:workspace/id workspace)
                                    (get-in prepared
                                            [:receipt :workspace/ids :solver]))
                                 (= (select-keys (get-in prepared
                                                        [:receipt :seat/ids])
                                                [:solver :proctor])
                                    (into {} (map (fn [[role seat]]
                                                    [role (:agent-id seat)]))
                                          (select-keys seats [:solver :proctor]))))
                      {:ok false :error/code :library-lane-preparation-drift
                       :findings [{:finding :prepared-authority-not-reobserved}]}
                      {:ok true :config config})))))))))))
