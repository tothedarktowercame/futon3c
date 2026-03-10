(ns futon3c.dev.ct
  "CT work queue — PlanetMath wiring diagram extraction.

   Extracted from futon3c.dev (Phase 2 of TN-dev-clj-decomposition).
   Also includes Tickle orchestration REPL helpers (issue kicking,
   preflight checks, smoke tests) since they share the same dependencies."
  (:require [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.agents.tickle-work-queue :as ct-queue]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.util UUID]))

;; ---------------------------------------------------------------------------
;; Tickle orchestration — REPL helpers (issue kicking)
;; ---------------------------------------------------------------------------

(defn fetch-futon4-issues!
  "Fetch open Codex-labeled issues from futon4."
  []
  (orch/fetch-open-issues! "/home/joe/code/futon4" :label "codex"))

(defn fetch-ti-issues!
  "Fetch open tickle-integration issues from futon3c."
  []
  (orch/fetch-open-issues! "/home/joe/code/futon3c" :label "tickle-integration"))

(defn kick-all-ti-issues!
  "Kick all open tickle-integration issues to Codex sequentially."
  [evidence-store !irc-sys]
  (let [{:keys [ok issues error]} (fetch-ti-issues!)]
    (if ok
      (do (println "[dev] Found" (count issues) "TI issues:" (mapv :number issues))
          (orch/kick-queue! issues
            {:evidence-store evidence-store
             :repo-dir "/home/joe/code/futon3c"
             :send-to-channel! (when-let [s @!irc-sys]
                                 (:send-to-channel! (:server s)))
             :room "#futon"}))
      {:ok false :error error})))

(defn kick-ev-issue!
  "Kick a single futon4 EV issue to Codex."
  [evidence-store !irc-sys issue-number]
  (let [{:keys [ok issue error]} (orch/fetch-issue! "/home/joe/code/futon4" issue-number)]
    (if ok
      (orch/kick! issue
        {:evidence-store evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

(defn kick-all-ev-issues!
  "Kick all open Codex-labeled futon4 issues sequentially."
  [evidence-store !irc-sys]
  (let [{:keys [ok issues error]} (fetch-futon4-issues!)]
    (if ok
      (orch/kick-queue! issues
        {:evidence-store evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

(defn run-ev-issue!
  "Run full Tickle orchestration (assign + review) for a single futon4 EV issue."
  [evidence-store !irc-sys issue-number]
  (let [{:keys [ok issue error]} (orch/fetch-issue! "/home/joe/code/futon4" issue-number)]
    (if ok
      (orch/run-issue-workflow! issue
        {:evidence-store evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

(defn run-all-ev-issues!
  "Run Tickle orchestration for all open Codex-labeled futon4 issues."
  [evidence-store !irc-sys]
  (let [{:keys [ok issues error]} (fetch-futon4-issues!)]
    (if ok
      (orch/run-batch! issues
        {:evidence-store evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

;; ---------------------------------------------------------------------------
;; Tickle smoke tests — preflight + definitive runs
;; ---------------------------------------------------------------------------

(defn tickle-preflight!
  "Check all prerequisites for a Tickle orchestration run."
  [evidence-store !irc-sys]
  (let [agents (reg/registered-agents)
        agent-ids (set (map :id/value agents))]
    {:agents-registered (vec agent-ids)
     :codex-available?  (contains? agent-ids (config/configured-codex-agent-id))
     :claude-available? (contains? agent-ids "claude-1")
     :evidence-store?   (some? evidence-store)
     :irc?              (some? @!irc-sys)}))

(defn tickle-smoke!
  "Run a minimal Tickle smoke test with a synthetic issue."
  [evidence-store !irc-sys
   & {:keys [agent-id repo-dir timeout-ms dry-run?]
      :or {agent-id (config/configured-codex-agent-id)
           repo-dir "/home/joe/code/futon3c"
           timeout-ms 600000}}]
  (let [preflight (tickle-preflight! evidence-store !irc-sys)
        smoke-issue {:number 0
                     :title "Tickle smoke test"
                     :body (str "## Smoke Test\n\n"
                                "Create the file `test/futon3c/agents/tickle_smoke_output.txt` "
                                "containing exactly the text `SMOKE OK`.\n\n"
                                "This is an automated test of the Tickle orchestration pipeline. "
                                "Do not modify any other files.\n\n"
                                "## Criteria\n\n"
                                "- [ ] File exists at test/futon3c/agents/tickle_smoke_output.txt\n"
                                "- [ ] File contains exactly `SMOKE OK`\n"
                                "- [ ] No other files modified")
                     :labels ["smoke-test"]}
        config {:evidence-store evidence-store
                :repo-dir repo-dir
                :agent-id agent-id
                :timeout-ms timeout-ms
                :send-to-channel! (when-let [s @!irc-sys]
                                    (:send-to-channel! (:server s)))
                :room "#futon"}]
    (if dry-run?
      {:dry-run true
       :preflight preflight
       :issue smoke-issue
       :config (dissoc config :evidence-store :send-to-channel!)}
      (do
        (println "[smoke] Preflight:" preflight)
        (println "[smoke] Kicking" agent-id "with smoke issue...")
        (let [result (orch/kick! smoke-issue config)
              output-file (java.io.File. (str repo-dir "/test/futon3c/agents/tickle_smoke_output.txt"))
              file-ok? (and (.exists output-file)
                            (= "SMOKE OK" (str/trim (slurp output-file))))]
          (println "[smoke] Result:" (:status result)
                   (if file-ok? "— output file verified" "— output file NOT found"))
          (assoc result
                 :smoke-file-ok? file-ok?
                 :preflight preflight))))))

(defn tickle-verify!
  "Query evidence store for recent orchestration evidence."
  [evidence-store & {:keys [limit tag] :or {limit 20 tag :orchestrate}}]
  (let [entries (estore/query* evidence-store {})
        orch-entries (->> entries
                         (filter #(some #{tag} (:evidence/tags %)))
                         (sort-by :evidence/at)
                         (take-last limit))]
    (println "[verify]" (count orch-entries) "orchestration evidence entries found")
    (doseq [e orch-entries]
      (println "  " (:evidence/at e)
               (last (:evidence/tags e))
               (select-keys (:evidence/body e) [:agent :ok :status :issue-number])))
    orch-entries))

(defn tickle-status!
  "Pretty-print the current Tickle orchestration status summary."
  [evidence-store]
  (let [status-fn (or (ns-resolve 'futon3c.agents.tickle-orchestrate 'tickle-status)
                      (do
                        (require 'futon3c.agents.tickle-orchestrate :reload)
                        (ns-resolve 'futon3c.agents.tickle-orchestrate 'tickle-status)))]
    (when-not status-fn
      (throw (ex-info "tickle-status helper unavailable"
                      {:ns 'futon3c.agents.tickle-orchestrate
                       :var 'tickle-status})))
    (let [summary (status-fn evidence-store)]
      (println "[dev] Tickle orchestration status:")
      (pprint/pprint summary)
      summary)))

(defn tickle-report!
  "Emit a one-line Tickle status report (IRC + evidence)."
  [evidence-store !irc-sys
   & {:keys [room repo-dir now]
      :or {room "#futon"
           repo-dir "/home/joe/code/futon3c"}}]
  (let [send-fn (or (some-> @!irc-sys :server :send-to-channel!)
                    (dev-irc/make-irc-send-fn "tickle-1"))]
    (if-not evidence-store
      (do
        (println "[dev] No evidence store available; cannot emit Tickle report.")
        {:ok false :error :missing-evidence-store})
      (if-let [report-status! (or (ns-resolve 'futon3c.agents.tickle-orchestrate 'report-status!)
                                  (do
                                    (require 'futon3c.agents.tickle-orchestrate :reload)
                                    (ns-resolve 'futon3c.agents.tickle-orchestrate 'report-status!)))]
        (let [result (report-status!
                      {:evidence-store evidence-store
                       :send-to-channel! send-fn
                       :room room
                       :repo-dir repo-dir
                       :now now})]
          (println "[dev] Tickle report:" (:message result))
          result)
        (do
          (println "[dev] Tickle report unavailable: report-status! missing")
          {:ok false :error :missing-report-status})))))

;; ---------------------------------------------------------------------------
;; CT work queue
;; ---------------------------------------------------------------------------

(defn ct-progress!
  "Show CT work queue progress: how many of 313 entries have been processed."
  [evidence-store]
  (let [status (ct-queue/queue-status evidence-store)]
    (println (str "[ct] Progress: " (:completed status) "/" (:total status)
                  " completed, " (:remaining status) " remaining"))
    status))

(defn run-ct-entry!
  "Process a single CT entity through the extract→review pipeline."
  [evidence-store !irc-sys
   & {:keys [entity-id agent-id timeout-ms review? review-timeout-ms]
      :or {agent-id (config/configured-codex-agent-id)
           timeout-ms 300000
           review? true
           review-timeout-ms 300000}}]
  (let [send-fn (or (some-> @!irc-sys :server :send-to-channel!)
                    (dev-irc/make-irc-send-fn "tickle-1"))
        issue (if entity-id
                (let [entities (ct-queue/load-ct-entities)
                      idx (.indexOf (mapv :entity-id entities) entity-id)]
                  (when (>= idx 0)
                    (ct-queue/entity->issue (nth entities idx) idx)))
                (first (ct-queue/next-unprocessed evidence-store 1)))]
    (if-not issue
      (do (println "[ct] No entries to process"
                   (if entity-id (str "(entity " entity-id " not found)") "(queue complete)"))
          {:ok false :error :no-entries})
      (let [session-id (str "ct-" (UUID/randomUUID))
            eid (:entity-id issue)]
        (println (str "[ct] Processing: " eid " — " (:title issue)))
        (ct-queue/emit-ct-evidence! evidence-store
                                    {:entity-id eid
                                     :entity-type (:entity-type issue)
                                     :session-id session-id
                                     :event-tag :workflow-start
                                     :ground-truth (:ground-truth issue)})
        (println (str "[ct] Assigning to " agent-id "..."))
        (let [extract-result (orch/assign-issue! issue
                                                  {:evidence-store evidence-store
                                                   :repo-dir "/home/joe/code/futon6"
                                                   :agent-id agent-id
                                                   :timeout-ms timeout-ms
                                                   :session-id session-id})]
          (if-not (:ok extract-result)
            (do
              (println (str "[ct] Extraction failed: " (:error extract-result)))
              (ct-queue/emit-ct-evidence! evidence-store
                                          {:entity-id eid
                                           :entity-type (:entity-type issue)
                                           :session-id session-id
                                           :event-tag :workflow-complete
                                           :ground-truth (:ground-truth issue)})
              {:ok false :entity-id eid :error (:error extract-result)})
            (let [extraction (:result extract-result)]
              (println (str "[ct] Extraction complete (" (:elapsed-ms extract-result) "ms)"))
              (ct-queue/emit-ct-evidence! evidence-store
                                          {:entity-id eid
                                           :entity-type (:entity-type issue)
                                           :session-id session-id
                                           :event-tag :extraction-complete
                                           :extraction-result extraction
                                           :ground-truth (:ground-truth issue)})
              (if-not review?
                (do
                  (ct-queue/emit-ct-evidence! evidence-store
                                              {:entity-id eid
                                               :entity-type (:entity-type issue)
                                               :session-id session-id
                                               :event-tag :workflow-complete
                                               :extraction-result extraction
                                               :ground-truth (:ground-truth issue)})
                  (when send-fn
                    (send-fn "#futon" "tickle-1"
                             (str "CT extracted: " eid " (" (:elapsed-ms extract-result) "ms)")))
                  {:ok true :entity-id eid :status :extracted
                   :elapsed-ms (:elapsed-ms extract-result)})
                (let [entities (ct-queue/load-ct-entities)
                      entity (first (filter #(= eid (:entity-id %)) entities))
                      review-prompt (ct-queue/make-review-prompt entity extraction)
                      review-issue {:number (:number issue)
                                    :title (str "CT-review: " (:title issue))
                                    :body review-prompt}]
                  (println "[ct] Requesting Claude review...")
                  (let [review-result (orch/request-review! review-issue extract-result
                                                             {:evidence-store evidence-store
                                                              :repo-dir "/home/joe/code/futon6"
                                                              :timeout-ms review-timeout-ms
                                                              :session-id session-id})
                        verdict (or (:verdict review-result) :unclear)]
                    (println (str "[ct] Review: " (name verdict)
                                  " (" (:elapsed-ms review-result) "ms)"))
                    (ct-queue/emit-ct-evidence! evidence-store
                                                {:entity-id eid
                                                 :entity-type (:entity-type issue)
                                                 :session-id session-id
                                                 :event-tag :workflow-complete
                                                 :extraction-result extraction
                                                 :verdict (name verdict)
                                                 :ground-truth (:ground-truth issue)})
                    (when send-fn
                      (send-fn "#futon" "tickle-1"
                               (str "CT " eid ": " (name verdict)
                                    " (extract " (:elapsed-ms extract-result) "ms"
                                    ", review " (:elapsed-ms review-result) "ms)")))
                    {:ok true :entity-id eid :status :reviewed
                     :verdict verdict
                     :extract-elapsed-ms (:elapsed-ms extract-result)
                     :review-elapsed-ms (:elapsed-ms review-result)}))))))))))

(defn run-ct-batch!
  "Process N CT entries overnight. Resumable — skips already-processed entries."
  [evidence-store !irc-sys
   & {:keys [n cooldown-ms agent-id timeout-ms review?]
      :or {n 10 cooldown-ms 5000 agent-id (config/configured-codex-agent-id)
           timeout-ms 300000 review? true}}]
  (let [issues (ct-queue/next-unprocessed evidence-store n)
        total (count issues)
        start (System/currentTimeMillis)]
    (println (str "[ct-batch] Starting: " total " entries"
                  " (agent=" agent-id
                  " review=" review?
                  " cooldown=" cooldown-ms "ms)"))
    (when (some-> @!irc-sys :server :send-to-channel!)
      (let [send-fn (:send-to-channel! (:server @!irc-sys))]
        (send-fn "#futon" "tickle-1"
                 (str "CT batch starting: " total " entries"))))
    (let [results
          (reduce
           (fn [acc [idx issue]]
             (println (str "\n[ct-batch] " (inc idx) "/" total
                           " — " (:entity-id issue)))
             (let [result (run-ct-entry!
                           evidence-store !irc-sys
                           :entity-id (:entity-id issue)
                           :agent-id agent-id
                           :timeout-ms timeout-ms
                           :review? review?)]
               (when (and (< (inc idx) total) (pos? cooldown-ms))
                 (Thread/sleep cooldown-ms))
               (conj acc result)))
           []
           (map-indexed vector issues))
          elapsed (- (System/currentTimeMillis) start)
          ok-count (count (filter :ok results))
          fail-count (- total ok-count)]
      (println (str "\n[ct-batch] Complete: " ok-count "/" total " succeeded"
                    " (" fail-count " failed)"
                    " in " (long (/ elapsed 1000)) "s"))
      (when (some-> @!irc-sys :server :send-to-channel!)
        (let [send-fn (:send-to-channel! (:server @!irc-sys))]
          (send-fn "#futon" "tickle-1"
                   (str "CT batch complete: " ok-count "/" total
                        " in " (long (/ elapsed 60000)) "min"))))
      {:total total
       :ok ok-count
       :failed fail-count
       :elapsed-ms elapsed
       :results results})))
