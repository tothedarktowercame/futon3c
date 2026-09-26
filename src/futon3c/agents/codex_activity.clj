(ns futon3c.agents.codex-activity
  "Attribute completed Codex file-change receipts to their admitted turn.
   Accept both exec NDJSON and the event_msg envelope retained in rollouts."
  (:require [clojure.java.io :as io]
            [futon3c.agency.clock-decision :as decision]))

(defn file-writes
  "Extract witnessed writes, never proposed patches or command text."
  [event cwd]
  (let [payload (if (= "event_msg" (:type event)) (:payload event) event)
        item (:item payload)
        changes (:changes item)
        pairs (if (map? changes)
                (map (fn [[path change]] [(if (keyword? path) (subs (str path) 1) path) change]) changes)
                (map (fn [change] [(:path change) change]) changes))]
    (when (and (contains? #{"item.completed" "item_completed"} (:type payload))
               (contains? #{"file_change" "FileChange"} (:type item))
               (= "completed" (:status item))
               (seq (:id item)))
      (vec
       (for [[path change] pairs
             path (distinct (remove nil? [path (:move_path change)]))
             :when (and (string? path) (seq path))
             :let [file (io/file path)
                   absolute (if (.isAbsolute file) file (io/file cwd path))]]
         {:path (.getCanonicalPath absolute)
          :item-id (:id item)
          :session-id (:thread_id payload)})))))

(defn make-consumer
  "One consumer per CLI invocation, retaining its exact admission context.
   Stable item/path identity makes duplicate stdout/stderr receipts idempotent."
  [cwd]
  (let [context decision/*turn*
        session (atom (:session-id context))]
    (fn [event]
      (when context
        (when (= "thread.started" (:type event))
          (reset! session (or (:thread_id event) (:session_id event) @session)))
        (doseq [{:keys [path item-id session-id]} (file-writes event cwd)]
          (binding [decision/*turn* context]
            (decision/record-tool-use!
             (:agent-id context) (or session-id @session)
             {:name "Edit" :id (str "codex-file-change:" item-id ":" path)
              :input {:file_path path}})))))))
