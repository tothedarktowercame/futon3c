(ns futon3c.dev.arse
  "ArSE work queue — Artificial Stack Exchange synthetic QA generation.

   Extracted from futon3c.dev (Phase 2 of TN-dev-clj-decomposition).
   Manages the generate→review pipeline for ArSE work items."
  (:require [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.agents.arse-work-queue :as arse-queue]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc])
  (:import [java.util UUID]))

(defn arse-progress!
  "Show ArSE work queue progress: how many work items have been processed."
  [evidence-store]
  (let [status (arse-queue/queue-status evidence-store)]
    (println (str "[arse] Progress: " (:completed status) "/" (:total status)
                  " completed, " (:remaining status) " remaining"))
    (doseq [[p stats] (:by-problem status)]
      (println (str "  P" p ": " (:done stats) "/" (:total stats)
                    " (" (:remaining stats) " remaining)")))
    status))

(defn run-arse-entry!
  "Process a single ArSE work item through the generate→review pipeline."
  [evidence-store !irc-sys
   & {:keys [entity-id agent-id timeout-ms review? review-timeout-ms]
      :or {agent-id (config/configured-codex-agent-id)
           timeout-ms 300000
           review? true
           review-timeout-ms 300000}}]
  (let [send-fn (or (some-> @!irc-sys :server :send-to-channel!)
                    (dev-irc/make-irc-send-fn "tickle-1"))
        issue (if entity-id
                (let [entities (arse-queue/load-arse-entities)
                      eid-str (if (keyword? entity-id) (name entity-id) (str entity-id))
                      idx (.indexOf (mapv :entity-id-str entities) eid-str)]
                  (when (>= idx 0)
                    (arse-queue/entity->issue (nth entities idx) idx)))
                (first (arse-queue/next-unprocessed evidence-store 1)))]
    (if-not issue
      (do (println "[arse] No entries to process"
                   (if entity-id (str "(entity " entity-id " not found)") "(queue complete)"))
          {:ok false :error :no-entries})
      (let [session-id (str "arse-" (UUID/randomUUID))
            eid (:entity-id issue)]
        (println (str "[arse] Processing: " eid " — " (:title issue)))
        (arse-queue/emit-arse-evidence! evidence-store
                                      {:entity-id eid
                                       :node-id (:node-id issue)
                                       :problem (:problem issue)
                                       :instance (:instance issue)
                                       :session-id session-id
                                       :event-tag :workflow-start})
        (println (str "[arse] Assigning to " agent-id "..."))
        (let [gen-result (orch/assign-issue! issue
                                              {:evidence-store evidence-store
                                               :repo-dir "/home/joe/code/futon6"
                                               :agent-id agent-id
                                               :timeout-ms timeout-ms
                                               :session-id session-id})]
          (if-not (:ok gen-result)
            (do
              (println (str "[arse] Generation failed: " (:error gen-result)))
              (arse-queue/emit-arse-evidence! evidence-store
                                            {:entity-id eid
                                             :node-id (:node-id issue)
                                             :problem (:problem issue)
                                             :instance (:instance issue)
                                             :session-id session-id
                                             :event-tag :workflow-complete})
              {:ok false :entity-id eid :error (:error gen-result)})
            (let [generation (:result gen-result)]
              (println (str "[arse] Generation complete (" (:elapsed-ms gen-result) "ms)"))
              (arse-queue/emit-arse-evidence! evidence-store
                                            {:entity-id eid
                                             :node-id (:node-id issue)
                                             :problem (:problem issue)
                                             :instance (:instance issue)
                                             :session-id session-id
                                             :event-tag :generation-complete
                                             :generation-result generation})
              (if-not review?
                (do
                  (arse-queue/emit-arse-evidence! evidence-store
                                                {:entity-id eid
                                                 :node-id (:node-id issue)
                                                 :problem (:problem issue)
                                                 :instance (:instance issue)
                                                 :session-id session-id
                                                 :event-tag :workflow-complete
                                                 :generation-result generation})
                  (when send-fn
                    (send-fn "#futon" "tickle-1"
                             (str "ArSE generated: " eid " (" (:elapsed-ms gen-result) "ms)")))
                  {:ok true :entity-id eid :status :generated
                   :elapsed-ms (:elapsed-ms gen-result)})
                (let [entities (arse-queue/load-arse-entities)
                      entity (first (filter #(= eid (:entity-id %)) entities))
                      review-prompt (arse-queue/make-review-prompt entity generation)
                      review-issue {:number (:number issue)
                                    :title (str "ArSE-review: " (:title issue))
                                    :body review-prompt}]
                  (println "[arse] Requesting Claude review...")
                  (let [review-result (orch/request-review! review-issue gen-result
                                                             {:evidence-store evidence-store
                                                              :repo-dir "/home/joe/code/futon6"
                                                              :timeout-ms review-timeout-ms
                                                              :session-id session-id})
                        verdict (or (:verdict review-result) :unclear)]
                    (println (str "[arse] Review: " (name verdict)
                                  " (" (:elapsed-ms review-result) "ms)"))
                    (arse-queue/emit-arse-evidence! evidence-store
                                                  {:entity-id eid
                                                   :node-id (:node-id issue)
                                                   :problem (:problem issue)
                                                   :instance (:instance issue)
                                                   :session-id session-id
                                                   :event-tag :workflow-complete
                                                   :generation-result generation
                                                   :verdict (name verdict)})
                    (when send-fn
                      (send-fn "#futon" "tickle-1"
                               (str "ArSE " eid ": " (name verdict)
                                    " (gen " (:elapsed-ms gen-result) "ms"
                                    ", review " (:elapsed-ms review-result) "ms)")))
                    {:ok true :entity-id eid :status :reviewed
                     :verdict verdict
                     :gen-elapsed-ms (:elapsed-ms gen-result)
                     :review-elapsed-ms (:elapsed-ms review-result)}))))))))))

(defn run-arse-batch!
  "Process N ArSE work items overnight. Resumable — skips already-processed entries."
  [evidence-store !irc-sys
   & {:keys [n cooldown-ms agent-id timeout-ms review? problem]
      :or {n 10 cooldown-ms 5000 agent-id (config/configured-codex-agent-id)
           timeout-ms 300000 review? true}}]
  (let [all-issues (arse-queue/next-unprocessed evidence-store 1000)
        issues (cond->> all-issues
                 problem (filter #(= problem (:problem %)))
                 true (take n))
        total (count issues)
        start (System/currentTimeMillis)]
    (println (str "[arse-batch] Starting: " total " items"
                  (when problem (str " (P" problem ")"))
                  " (agent=" agent-id
                  " review=" review?
                  " cooldown=" cooldown-ms "ms)"))
    (when (some-> @!irc-sys :server :send-to-channel!)
      (let [send-fn (:send-to-channel! (:server @!irc-sys))]
        (send-fn "#futon" "tickle-1"
                 (str "ArSE batch starting: " total " items"
                      (when problem (str " (P" problem ")"))))))
    (let [results
          (reduce
           (fn [acc [idx issue]]
             (println (str "\n[arse-batch] " (inc idx) "/" total
                           " — " (:entity-id issue)))
             (let [result (run-arse-entry!
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
      (println (str "\n[arse-batch] Complete: " ok-count "/" total " succeeded"
                    " (" fail-count " failed)"
                    " in " (long (/ elapsed 1000)) "s"))
      (when (some-> @!irc-sys :server :send-to-channel!)
        (let [send-fn (:send-to-channel! (:server @!irc-sys))]
          (send-fn "#futon" "tickle-1"
                   (str "ArSE batch complete: " ok-count "/" total
                        " in " (long (/ elapsed 60000)) "min"))))
      {:total total
       :ok ok-count
       :failed fail-count
       :elapsed-ms elapsed
       :results results})))
