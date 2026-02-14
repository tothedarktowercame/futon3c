(ns futon3c.peripheral.edit
  "Edit peripheral implementation.

   Constraints: edit/write tools scoped to allowed paths.
   Fruit: {:changes n, :changed-files [...], :ready-to-test? bool}
   Exit context: {:session-id ..., :changed-files [...]}."
  (:require [clojure.string :as str]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(defn- arg-files
  [tool args]
  (case tool
    ;; :edit takes [file-path old-string new-string] — only first is a file
    ;; :write takes [file-path content] — only first is a file
    (:edit :write) (when (string? (first args)) [(first args)])
    []))

(defn- result-files
  [result]
  (->> (cond
         (map? result)
         (mapcat (fn [k]
                   (let [v (get result k)]
                     (cond
                       (string? v) [v]
                       (sequential? v) (filter string? v)
                       :else [])))
                 [:changed-files :files :paths :path :file])
         :else [])
       (remove str/blank?)))

(defn- changed-files
  [tool args result]
  (->> (concat (arg-files tool args) (result-files result))
       distinct
       vec))

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :edit action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          dispatch-result (tools/dispatch-tool tool args spec backend)]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :edit :tool-execution-failed
                             "Edit tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              files (changed-files tool args result)
              ev (evidence/make-step-evidence
                  :edit (:session-id state) (:author state)
                  tool args result (:last-evidence-id state))
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (update :steps conj {:tool tool :args args :result result})
                            (update :changed-files #(-> (concat % files) distinct vec)))
              append-err (common/maybe-append-evidence! new-state ev)]
          (if append-err
            append-err
            {:ok true
             :state new-state
             :result result
             :evidence ev}))))))

(defrecord EditPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :edit context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :edit sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :target-files (vec (or (:target-files context) []))
                   :changed-files []
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [files (vec (distinct (:changed-files state)))
          fruit {:changes (count files)
                 :changed-files files
                 :ready-to-test? (pos? (count files))}
          ev (evidence/make-stop-evidence
              :edit
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context {:session-id (:session-id state)
                   :changed-files files}
         :fruit fruit
         :evidence ev}))))

(defn make-edit
  "Create an edit peripheral with injected backend."
  ([] (make-edit (tools/make-mock-backend)))
  ([backend]
   (->EditPeripheral (common/load-spec :edit) backend))
  ([spec backend]
   (->EditPeripheral spec backend)))
