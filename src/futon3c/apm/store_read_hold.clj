(ns futon3c.apm.store-read-hold
  "Durable per-request warnings and an idempotent Agency repair handoff."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.substrate.read-health :as health])
)

(defn warnings-directory [directory]
  (io/file directory "store-read-warnings"))

(defn record-warning! [directory warning]
  (let [id (machine/ledger-digest [warning])
        path (.toPath (io/file (warnings-directory directory) (str id ".edn")))
        result (runtime/atomic-persist! path (assoc warning :warning/id id))]
    (when-not (:ok result)
      (throw (ex-info "Store read warning could not be retained"
                      {:error/code :store-read-warning-persistence-failed})))
    id))

(defn with-frame! [policy frame directory f]
  (if-not policy
    (f)
    (do
      (when-not (and (= 1 (:policy/version policy))
                     (string? (:repair-agent-id policy))
                     (seq (:repair-agent-id policy)))
        (throw (ex-info "Store read health policy requires a repair agent"
                        {:error/code :store-read-health-policy-invalid})))
      (binding [health/*context* (select-keys frame [:frame/id :problem/id])
                health/*record-warning!* #(record-warning! directory %)]
        (f)))))

(defn warnings [directory frame]
  (let [dir (warnings-directory directory)
        children (.listFiles dir)
        _ (when (and (.exists dir) (nil? children))
            (throw (ex-info "Store warnings directory is unreadable"
                            {:error/code :store-read-warning-directory-unreadable})))
        files (->> (or children [])
                   (filter #(.endsWith (.getName ^java.io.File %) ".edn"))
                   (sort-by #(.getName ^java.io.File %)))
        records (mapv #(edn/read-string (slurp %)) files)]
    (doseq [record records]
      (when-not (and (= (:frame/id frame) (:frame/id record))
                     (= (:problem/id frame) (:problem/id record))
                     (= (:warning/id record)
                        (machine/ledger-digest [(dissoc record :warning/id)])))
        (throw (ex-info "Store warning identity mismatch"
                        {:error/code :store-read-warning-identity-invalid}))))
    records))

(defn dispatch!
  "Uses a stable announced job id. Re-observe before activation so a completed
  repair is never reactivated after a crash before the dispatch receipt write."
  [http-fn agency-base policy hold]
  (let [agent-id (:repair-agent-id policy)
        job-id (str "store-repair-" (:hold/id hold))
        request {:agent-id agent-id :job-id job-id :surface "bell"
                 :caller "apm-store-read-monitor"
                 :prompt
                 (str "The APM queue has completed and retired this frame, then held "
                      "before its successor because of slow store reads. Joe commissions "
                      "you to diagnose and repair the underlying substrate, validate the "
                      "repair, then resume this held queue. Do not rerun the completed "
                      "proof, weaken evidence checks, erase warnings, or raise the "
                      "warning threshold to make the hold disappear. Do not restart "
                      "JVMs or V2/topology loops. Read the committed store-read hold "
                      "runbook in holes/labs/M-apm-demonstration/analysis/"
                      "store-read-warning-hold-2026-09-11/README.md. "
                      "A completed bell alone does not release the queue. If repair "
                      "is not verified, retain the hold and report the concrete blocker. "
                      "Read the complete warnings from that queue's :store-read/hold. "
                      "Authority: " (pr-str {:hold (assoc (dissoc hold :warnings)
                                                                        :warning/count (count (:warnings hold))
                                                                        :warning/examples (vec (take 5 (:warnings hold))))
                                               :queue-state-path (:queue-state-path policy)
                                               :coordinator-id (:coordinator-id policy)}))}]
    (if-not (and (string? agent-id) (seq agent-id))
      {:ok false :error/code :store-repair-agent-missing}
      (let [announced (job-port/announce! http-fn agency-base request)]
        (if-not (and (:ok announced) (= job-id (:job-id announced)))
          {:ok false :error/code :store-repair-announcement-failed}
          (let [observed (job-port/observe http-fn agency-base job-id)]
            (cond
              (not (and (:ok observed) (= agent-id (:agent-id observed))))
              {:ok false :error/code :store-repair-identity-unverified}
              (or (:terminal? observed)
                  (contains? #{:running :overrun :delivered :delivering} (:state observed)))
              {:ok true :dispatch/id job-id :repair/agent-id agent-id}
              :else
              (let [activated (job-port/activate! http-fn agency-base request)]
                (if (:ok activated)
                  {:ok true :dispatch/id job-id :repair/agent-id agent-id}
                  {:ok false :error/code :store-repair-activation-failed})))))))))
