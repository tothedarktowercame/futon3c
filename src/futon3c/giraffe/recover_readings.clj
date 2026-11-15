(ns futon3c.giraffe.recover-readings
  "One-off maintenance: land discarded Giraffe readings from stored Agency
  answers instead of re-reading.

  Background (Joe, 2026-11-15): 29 valid readings failed as
  :reader-record-invalid-json because zai-2 prefixed a prose sentence to its
  answers; the parser is fixed (futon3c c463df90c) but the answers were
  already discarded. Each dispatch is pinned in the requisition ledger
  (~/code/giraffe-reread-requisitions, one E-giraffe-read-N.md per reading,
  carrying Target + source sha + job id + terminal state), so a failed
  request's stored answer can be found again: re-parse it with the fixed
  parser and land it through the worker's normal checks (pin, checker,
  generation) exactly as if it had just arrived. Requests whose job was
  REFUSED before any reading (Kimi requisition refusals, :agency-job-failed)
  did no work: they are requeued as pending, which is not repeated work.

  Answer sources, in order: the live Agency job (transcripts are compacted
  after 24 h and dropped after 7), then the reader's scratch file at
  ~/code/storage/giraffe-reread-scratch/<target>-<sha8>.jsonl (the reading
  prompt makes the reader write its self-checked record there), which is what
  recovered the 2026-11-15 batch. Dry-run is the default; pass --apply to
  write. Never dispatches readers."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.reread-cli :as cli]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.reread-worker :as worker]))

(def default-agency-url "http://localhost:7070")
(def default-ledger-dir
  "/home/joe/code/giraffe-reread-requisitions/holes/excursions")

(defn default-fetch-job
  "The stored Agency job map for JOB-ID, or nil."
  [agency-url job-id]
  (try
    (let [response (json/parse-string
                    (slurp (str agency-url "/api/alpha/invoke/jobs/" job-id)) true)]
      (or (:job response) response))
    (catch Exception _ nil)))

(defn ledger-index
  "Index DIR's requisition files by reading claim: {(target-id, source-sha)
  [{:job job-id :state terminal-state-or-in-progress}]}, newest last."
  [dir]
  (reduce (fn [index file]
            (let [text (try (slurp file) (catch Exception _ ""))]
              (if-let [target (second (re-find #"(?m)^Target: (\S+)" text))]
                (if-let [sha (second (re-find #"(?m)^Required source sha256: (\S+)" text))]
                  (let [entry (if-let [[_ job state]
                                       (re-find #"(?s)\*\*Requisition:\*\*.*?job (invoke-[0-9]+-[0-9]+-[0-9a-f]+), state (\w+)"
                                                 text)]
                                {:job job :state state}
                                {:job nil :state :in-progress})]
                    (update index [target sha] (fnil conj []) entry))
                  index)
                index)))
          {}
          (->> (io/file dir) file-seq (filter #(.isFile %))
               (filter #(re-matches #"E-[^/]+\.md" (.getName %)))
               (sort-by str))))

(defn- claim-failed-row!
  "Move REQUEST (state failed) to claimed for the recovery reader so settle!
  accepts the outcome. Nil when another writer changed it first."
  [store request now-ms]
  (queue/reclaim-for-retry! store request "zai-5-recovery" now-ms (* 10 60 1000)))

(defn- land-record
  "Land RECORD for REQUEST through the normal publish+settle path.
  DRY-RUN? reports without writing. Returns a result map."
  [{:keys [store pointer generations-dir now-ms max-attempts dry-run?]}
   {:keys [request record job]}]
  (if-not (worker/record-matches-claim record request)
    {:outcome :still-failed :reason :reader-record-pin-mismatch
     :target (:target_id request) :job job}
    (if dry-run?
      {:outcome :would-land :target (:target_id request) :job job}
      (if-let [claimed (claim-failed-row! store request now-ms)]
        (let [published (generation/publish-generation!
                         {:pointer pointer :generations-dir generations-dir
                          :claim {:target-id (:target_id request)
                                  :source-path (:source_path request)
                                  :source-sha (:source_sha request)
                                  :old-sha (:old_sha request)}
                          :replacement record})]
          (if (= :published (:status published))
            (do (queue/settle! store claimed
                               {:status :landed :digest (:digest published)}
                               now-ms (or max-attempts worker/default-max-attempts))
                {:outcome :recovered :target (:target_id request) :job job
                 :digest (:digest published)})
            (let [superseded? (= :giraffe-generation/source-changed (:reason published))]
              (queue/settle! store claimed
                             {:status (if superseded? :superseded :failed)
                              :reason (:reason published)
                              :answer-text (json/generate-string record)}
                             now-ms (or max-attempts worker/default-max-attempts))
              {:outcome (if superseded? :superseded :still-failed)
               :reason (:reason published)
               :target (:target_id request) :job job})))
        {:outcome :still-failed :reason :recovery-claim-lost
         :target (:target_id request) :job job}))))

(defn recover!
  "Recover FAILED queue rows from stored Agency answers. Opts:
  :store :ledger-dir :pointer :generations-dir :fetch-job :agency-url
  :now-ms :dry-run? (default true). Returns a report; with :apply? true
  writes landings/requeues. Only rows in state failed are touched; rows
  whose last_error begins operator-withdrawn are skipped (operator intent)."
  [{:keys [store ledger-dir fetch-job agency-url now-ms dry-run?]
    :or {ledger-dir default-ledger-dir
         agency-url default-agency-url
         fetch-job default-fetch-job
         dry-run? true}
    :as opts}]
  (let [now-ms (long (or now-ms (System/currentTimeMillis)))
        index (ledger-index ledger-dir)
        failed (filter #(and (= :failed (:state %))
                             ;; last_error arrives as a keyword: (str k) has a
                             ;; leading colon, so compare its name (claude-13 review)
                             (not (str/starts-with? (some-> (:last_error %) name str)
                                                    "operator-withdrawn")))
                       (queue/requests store))]
    (reduce
     (fn [report request]
       (let [jobs (get index [(:target_id request) (:source_sha request)])
             answers (concat
                       ;; the live Agency job, while its transcript is retained
                       (keep (fn [{:keys [job state]}]
                               (when (and job (= "done" state))
                                 (when-let [j (fetch-job agency-url job)]
                                   {:job job :text (worker/answer-text j)})))
                             (or jobs []))
                       ;; the reader's self-check scratch file (the prompt makes the
                       ;; reader write its record there); survives job compaction
                       (when-let [scratch (try (slurp (worker/scratch-path request))
                                               (catch Exception _ nil))]
                         (when-not (str/blank? scratch)
                           [{:job "scratch" :text scratch}])))
             landings (keep (fn [{:keys [job text]}]
                              (when-let [record (worker/parse-record text)]
                                {:record record :job job}))
                            answers)]
         (if (seq landings)
           (let [landed (some (fn [candidate]
                                (let [result (land-record (assoc opts
                                                                :now-ms now-ms
                                                                :dry-run? dry-run?)
                                                          (assoc candidate :request request))]
                                  (when (contains? #{:recovered :would-land
                                                     :superseded}
                                                   (:outcome result))
                                    result)))
                              landings)]
             (if landed
               (update report (:outcome landed) conj landed)
               (update report :still-failed
                       conj {:target (:target_id request)
                             :reason :no-record-passed-checks
                             :jobs (mapv :job answers)})))
           (if (and (= :agency-job-failed (:last_error request))
                    (seq jobs)
                    (every? #(not= "done" (:state %)) jobs))
             ;; the job was refused before any reading: no work was done, so
             ;; requeueing is not repeated work
             (if dry-run?
               (update report :would-requeue conj {:target (:target_id request)})
               (do (queue/requeue! store request now-ms)
                   (update report :requeued conj {:target (:target_id request)})))
             (update report (if (seq jobs) :still-failed :no-ledger-match)
                     conj {:target (:target_id request)
                           :reason (:last_error request)
                           :jobs (mapv #(when (:job %) (str (:job %) ":" (:state %))) jobs)})))))
     {:recovered [] :would-land [] :superseded [] :still-failed []
      :requeued [] :would-requeue [] :no-ledger-match []}
     failed)))

(defn -main [& args]
  (let [apply? (contains? (set args) "--apply")
        store (queue/open-store)
        report (recover! {:store store :apply? apply? :dry-run? (not apply?)
                          :pointer cli/default-pointer})
        summary (update-vals report count)]
    (println (json/generate-string summary {:pretty true}))
    (when (some (comp pos? count) (vals (select-keys report [:still-failed :no-ledger-match])))
      (println "detail:")
      (doseq [[k xs] (select-keys report [:still-failed :no-ledger-match]) :when (seq xs)]
        (println k (pr-str (mapv :target xs)))))
    (when apply? (System/exit 0))))
