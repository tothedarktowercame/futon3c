(ns futon3c.peripheral.night-shift
  "Night Shift peripheral.

   A constrained capability envelope for branch-isolated code modification.
   Night Shift is intentionally not a generic edit/deploy peripheral: each
   tool is a typed step in the branch → commit → push → PR path, with
   operator review preserved as the merge boundary."
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.night-shift-backend :as backend]
            [futon3c.peripheral.night-shift-shapes :as shapes]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(defn- tool-substantive?
  [tool]
  (contains? shapes/substantive-tools tool))

(defn- validate-cg
  [args-map]
  (let [cg-id (:consent-gate-event-id args-map)]
    (cond
      (not (string? cg-id))
      {:ok false
       :error "Night Shift substantive actions require :consent-gate-event-id"}

      (not (.startsWith cg-id "cg-"))
      {:ok false
       :error "Night Shift consent gates must start with cg-"
       :night-shift-invariant :one-cg-one-branch}

      :else
      {:ok true :consent-gate-event-id cg-id})))

(defn- normalize-args
  [args]
  (cond
    (and (= 1 (count args)) (map? (first args))) (first args)
    (even? (count args)) (apply hash-map args)
    :else {}))

(defn- invariant-error
  [code message & [data]]
  (cond-> {:ok false
           :night-shift-invariant code
           :error message}
    data (assoc :data data)))

(defn- ensure-repo-consistency
  [state repo-path]
  (if-let [active (:active-repo-path state)]
    (if (= active repo-path)
      nil
      (invariant-error :no-cross-repo
                       (str "Night Shift session is bound to repo " active
                            "; cannot switch to " repo-path)))
    nil))

(defn- enrich-args
  [state args-map]
  (cond-> args-map
    (:active-branch state) (assoc :expected-branch (:active-branch state))
    (:author state) (assoc :author (:author state))))

(defn- maybe-bind-branch
  [state tool args-map result]
  (if (contains? #{:branch-create :frame-provision} tool)
    (-> state
        (assoc :active-branch (get-in result [:result :branch]))
        (assoc :active-repo-path (or (get-in result [:result :repo-path])
                                     (:repo-path args-map)
                                     (:source-repo-path args-map)))
        (assoc :source-agenda (:agenda-id args-map))
        (assoc :source-recommendation (:source-recommendation args-map))
        (assoc :bound-consent-gate (:consent-gate-event-id args-map)))
    state))

(defn- maybe-track-test-result
  [state tool result]
  (if (= tool :run-tests)
    (assoc state :last-test-result (:result result))
    state))

(defn- maybe-track-pr
  [state tool result]
  (if (= tool :pr-create)
    (assoc state :last-pr (:result result))
    state))

(defn- maybe-track-files
  [state tool args-map result]
  (let [files (case tool
                :edit-file [(:path args-map)]
                :create-file [(:path args-map)]
                :repo-stage (:paths args-map)
                :repo-commit (get-in result [:result :staged-paths])
                nil)]
    (if (seq files)
      (update state :changed-files #(-> (concat (or % []) files) distinct vec))
      state)))

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :night-shift action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          args-map (normalize-args args)]
      (cond
        (and (tool-substantive? tool)
             (not (:ok (validate-cg args-map))))
        (validate-cg args-map)

        (and (:repo-path args-map)
             (ensure-repo-consistency state (:repo-path args-map)))
        (ensure-repo-consistency state (:repo-path args-map))

        (and (= tool :branch-create) (:active-branch state))
        (invariant-error :one-cg-one-branch
                         (str "Night Shift session already bound to branch "
                              (:active-branch state)
                              "; open a new shift for another branch."))

        (and (= tool :pr-create)
             (:ready-for-review? args-map)
             (not (true? (get-in state [:last-test-result :passed?]))))
        (invariant-error :test-hook-integrity
                         "Night Shift refuses ready-for-review PR creation before passing tests."
                         {:last-test-result (:last-test-result state)})

        :else
        (let [dispatch-result (tools/dispatch-tool tool
                                                   [(enrich-args state args-map)]
                                                   spec
                                                   backend)]
          (cond
            (common/social-error? dispatch-result)
            dispatch-result

            (not (:ok dispatch-result))
            (runner/runner-error :night-shift :tool-execution-failed
                                 "Night Shift tool execution failed"
                                 :tool tool
                                 :args args
                                 :result dispatch-result)

            :else
            (let [result (:result dispatch-result)
                  ev (evidence/make-step-evidence
                      :night-shift (:session-id state) (:author state)
                      tool args result (:last-evidence-id state))
                  new-state (-> state
                                (assoc :last-evidence-id (:evidence/id ev))
                                (update :steps conj {:tool tool :args args-map :result result})
                                (maybe-bind-branch tool args-map dispatch-result)
                                (maybe-track-test-result tool dispatch-result)
                                (maybe-track-pr tool dispatch-result)
                                (maybe-track-files tool args-map dispatch-result))
                  append-err (common/maybe-append-evidence! new-state ev)]
              (if append-err
                append-err
                {:ok true
                 :state new-state
                 :result result
                 :evidence ev}))))))))

(defrecord NightShiftPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :night-shift context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :night-shift sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :active-branch nil
                   :active-repo-path nil
                   :bound-consent-gate nil
                   :last-test-result nil
                   :last-pr nil
                   :changed-files []
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [fruit {:active-branch (:active-branch state)
                 :repo-path (:active-repo-path state)
                 :changed-files (vec (:changed-files state))
                 :last-test-result (:last-test-result state)
                 :last-pr (:last-pr state)
                 :steps-taken (count (:steps state))}
          ev (evidence/make-stop-evidence
              :night-shift
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
                   :active-branch (:active-branch state)
                   :repo-path (:active-repo-path state)}
         :fruit fruit
         :evidence ev}))))

(defn make-night-shift
  ([] (make-night-shift (backend/make-night-shift-backend)))
  ([bk]
   (->NightShiftPeripheral (common/load-spec :night-shift) bk))
  ([spec bk]
   (->NightShiftPeripheral spec bk)))

(defn spike-check
  []
  (let [spec (common/load-spec :night-shift)
        make-result (try (make-night-shift)
                         (catch Throwable t {:error t}))]
     {:valid-config? (and (= :night-shift (:peripheral/id spec))
                         (contains? (:peripheral/tools spec) :frame-provision)
                         (contains? (:peripheral/tools spec) :branch-create)
                         (contains? (:peripheral/tools spec) :pr-create))
     :make-night-shift-ok? (and (not (instance? Throwable make-result))
                                (not (:error make-result)))
     :spike-error (when (instance? Throwable make-result)
                    (.getMessage ^Throwable make-result))
     :verified-criteria [:envelope-file-exists
                         :spec-loads
                         :factory-returns-without-error]
     :deferred-criteria [:generic-hop-traffic
                         :live-pr-demonstration
                         :discard-bell-integration]}))
