(ns futon3c.peripheral.explore
  "Explore peripheral implementation.

   Constraints: read-only tools over full codebase scope.
   Fruit: {:found [...], :summary string}
   Exit context: {:session-id ..., :target-files [...]}."
  (:require [clojure.string :as str]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(defn- extract-paths
  [x]
  (cond
    (string? x) [x]
    (sequential? x) (mapcat extract-paths x)
    (map? x) (mapcat extract-paths (vals (select-keys x [:found :matches :files :paths :path :file :target-files])))
    :else []))

(defn- discover-targets
  [result]
  (->> (extract-paths result)
       (remove str/blank?)
       distinct
       vec))

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :explore action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          dispatch-result (tools/dispatch-tool tool args spec backend)]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :explore :tool-execution-failed
                             "Explore tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              found (discover-targets result)
              ev (evidence/make-step-evidence
                  :explore (:session-id state) (:author state)
                  tool args result (:last-evidence-id state))
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (update :steps conj {:tool tool :args args :result result})
                            (update :found #(-> (concat % found) distinct vec)))
              append-err (common/maybe-append-evidence! new-state ev)]
          (if append-err
            append-err
            {:ok true
             :state new-state
             :result result
             :evidence ev}))))))

(defrecord ExplorePeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :explore context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :explore sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :found []
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [found (vec (distinct (:found state)))
          summary (if (seq found)
                    (str "Found " (count found) " target file(s)")
                    (str "No concrete targets found in " (count (:steps state)) " step(s)"))
          fruit {:found found :summary summary}
          ev (evidence/make-stop-evidence
              :explore
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
                   :target-files found}
         :fruit fruit
         :evidence ev}))))

(defn make-explore
  "Create an explore peripheral with injected backend.
   Backend must satisfy ToolBackend."
  ([] (make-explore (tools/make-mock-backend)))
  ([backend]
   (->ExplorePeripheral (common/load-spec :explore) backend))
  ([spec backend]
   (->ExplorePeripheral spec backend)))
