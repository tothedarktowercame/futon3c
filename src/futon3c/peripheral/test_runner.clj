(ns futon3c.peripheral.test-runner
  "Test peripheral implementation.

   Constraints: read + test-command execution only.
   Fruit: {:result :pass/:fail/:flaky, :test-count n, :failures [...]}
   Exit context: {:session-id ...}."
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(defn- run-failed?
  [result]
  (cond
    (map? result)
    (or (= :fail (:result result))
        (= :fail (:status result))
        (false? (:ok result))
        (and (int? (:exit result)) (not (zero? (:exit result))))
        (seq (:failures result)))

    (string? result)
    (boolean (re-find #"(?i)fail|error" result))

    :else false))

(defn- run-flaky?
  [result]
  (cond
    (map? result)
    (or (= :flaky (:result result))
        (= :flaky (:status result))
        (true? (:flaky? result)))

    (string? result)
    (boolean (re-find #"(?i)flaky" result))

    :else false))

(defn- run-test-count
  [result]
  (cond
    (map? result)
    (cond
      (int? (:test-count result)) (:test-count result)
      (int? (:tests result)) (:tests result)
      (sequential? (:tests result)) (count (:tests result))
      :else 0)

    :else 0))

(defn- run-failures
  [result]
  (cond
    (map? result)
    (let [failures (:failures result)]
      (cond
        (string? failures) [failures]
        (sequential? failures) (->> failures (map str) vec)
        :else []))

    (string? result)
    (if (run-failed? result) [result] [])

    :else []))

(defn- summarize-tests
  [runs]
  (let [count* (->> runs (map run-test-count) (reduce + 0))
        failures (->> runs (mapcat run-failures) vec)
        result (cond
                 (some run-flaky? runs) :flaky
                 (or (seq failures) (some run-failed? runs)) :fail
                 :else :pass)]
    {:result result
     :test-count count*
     :failures failures}))

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :test action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          dispatch-result (tools/dispatch-tool tool args spec backend)]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :test :tool-execution-failed
                             "Test tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              ev (evidence/make-step-evidence
                  :test (:session-id state) (:author state)
                  tool args result (:last-evidence-id state))
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (update :steps conj {:tool tool :args args :result result})
                            (cond-> (= :bash-test tool)
                              (update :test-runs conj result)))
              append-err (common/maybe-append-evidence! new-state ev)]
          (if append-err
            append-err
            {:ok true
             :state new-state
             :result result
             :evidence ev}))))))

(defrecord TestPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :test context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :test sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :test-runs []
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [summary (summarize-tests (:test-runs state))
          ev (evidence/make-stop-evidence
              :test
              (:session-id state)
              (:author state)
              summary
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context {:session-id (:session-id state)}
         :fruit summary
         :evidence ev}))))

(defn make-test-runner
  "Create a test peripheral with injected backend."
  ([] (make-test-runner (tools/make-mock-backend)))
  ([backend]
   (->TestPeripheral (common/load-spec :test) backend))
  ([spec backend]
   (->TestPeripheral spec backend)))
