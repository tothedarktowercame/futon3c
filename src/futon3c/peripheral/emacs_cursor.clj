(ns futon3c.peripheral.emacs-cursor
  "Emacs cursor peripheral.

   Read-only embodiment surface for a WS-connected agent inside Emacs.
   The runner consumes editor snapshots and mode/focus updates, then emits
   server-pushed peripheral_event frames for the authenticated WS client."
  (:require [futon3c.peripheral.common :as common]
            [futon3c.agency.registry :as registry]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.transport.peripheral-events :as peripheral-events]))

(def ^:private allowed-modes
  #{:follow :observe :scout})

(defn- normalize-mode [raw]
  (cond
    (keyword? raw) raw
    (string? raw) (keyword raw)
    :else nil))

(defn- validate-tool! [spec tool args]
  (cond
    (not (tools/allowed? tool spec))
    (runner/runner-error :emacs-cursor :tool-not-allowed
                         (str "Tool " tool " is not in this peripheral's tool set")
                         :tool tool
                         :allowed (:peripheral/tools spec))

    (= tool :cursor-context)
    (when-not (map? (first args))
      (runner/runner-error :emacs-cursor :invalid-cursor-context
                           "cursor-context expects one snapshot map"
                           :args (vec args)))

    (= tool :cursor-mode)
    (let [mode (normalize-mode (first args))]
      (when-not (contains? allowed-modes mode)
        (runner/runner-error :emacs-cursor :invalid-cursor-mode
                             "cursor-mode must be one of :follow, :observe, :scout"
                             :args (vec args)
                             :allowed-modes (vec (sort allowed-modes)))))

    (= tool :cursor-focus)
    (when-not (map? (first args))
      (runner/runner-error :emacs-cursor :invalid-cursor-focus
                           "cursor-focus expects one focus map"
                           :args (vec args)))

    (= tool :cursor-minibuffer)
    (when-not (map? (first args))
      (runner/runner-error :emacs-cursor :invalid-cursor-minibuffer
                           "cursor-minibuffer expects one minibuffer payload map"
                           :args (vec args)))

    :else nil))

(defn- buffer-surface [state]
  (let [ctx (:latest-context state)
        buffer-raw (:buffer ctx)
        buffer (if (map? buffer-raw) buffer-raw {})
        user-cursor (or (:user-cursor ctx) {})
        agent-cursor (or (:agent-cursor ctx) {})
        visible-text (or (get-in buffer [:visible :text])
                         (:visible-text ctx)
                         "")
        clipped (if (> (count visible-text) 160)
                  (str (subs visible-text 0 160) "...")
                  visible-text)]
    {:mode (name (:mode state))
     :editor-id (:editor-id state)
     :buffer buffer
     :user-cursor user-cursor
     :agent-cursor agent-cursor
     :focus (:focus state)
     :debug (or (:debug ctx) {})
     :visible-preview clipped}))

(defn- minibuffer-surface [state]
  (or (:latest-minibuffer state)
      (get-in state [:latest-context :minibuffer])
      {}))

(defn- cursor-payload [state]
  (let [ctx (:latest-context state)
        payload {:mode (name (:mode state))
                 :editor-id (:editor-id state)
                 :focus (:focus state)
                 :buffer-surface (buffer-surface state)
                 :minibuffer-surface (minibuffer-surface state)}]
    (cond-> payload
      (map? ctx) (merge ctx))))

(defn- context-buffer-name [ctx]
  (let [buffer-raw (:buffer ctx)]
    (cond
      (string? buffer-raw) buffer-raw
      (map? buffer-raw) (or (:name buffer-raw) (:buffer-name buffer-raw))
      :else nil)))

(defn- emit-cursor-state! [state]
  (if-let [agent-id (:agent-id state)]
    (peripheral-events/send-peripheral-event!
     agent-id
     :emacs-cursor
     :cursor-state
     (cursor-payload state))
    false))

(defn- projection-source [state]
  (str "emacs-cursor:" (:editor-id state)))

(defn- report-surface-projection! [state]
  (when-let [agent-id (:agent-id state)]
    (registry/report-surface-projection!
     agent-id
     (projection-source state)
     {:surface "emacs-cursor"
      :peripheral-id :emacs-cursor
      :editor-id (:editor-id state)
      :mode (name (:mode state))
      :buffer-surface (buffer-surface state)
      :minibuffer-surface (minibuffer-surface state)
      :buffer-summary (or (get-in state [:latest-context :buffer-surface])
                          (:last-buffer state))
      :write-surface "minibuffer"
      :write-contract "Emit lines `MINIBUFFER: <text-or-json>` to write back to Emacs; structured commands include `{\"command\":\"eval-sexp\",\"sexp\":\"(...)\"}` and non-editing e2e `{\"command\":\"run-script\",\"steps\":[...]}`."
      :debug (get-in state [:latest-context :debug])})))

(defn- dispatch-step [spec state action]
  (if-let [err (common/validate-action :emacs-cursor action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)]
      (if-let [err (validate-tool! spec tool args)]
        err
        (let [[arg] args
              updates (case tool
                        :cursor-context {:latest-context arg
                                         :last-buffer (or (context-buffer-name arg)
                                                          (:last-buffer state))}
                        :cursor-mode {:mode (normalize-mode arg)}
                        :cursor-focus {:focus arg}
                        :cursor-minibuffer {:latest-minibuffer arg})
              new-state (-> state
                            (merge updates)
                            (update :steps conj {:tool tool :args args})
                            (update :updates-processed inc))
              _ (report-surface-projection! new-state)
              emitted? (emit-cursor-state! new-state)
              ev (evidence/make-step-evidence
                  :emacs-cursor
                  (:session-id state)
                  (:author state)
                  tool
                  args
                  {:accepted true
                   :emitted? emitted?
                   :mode (:mode new-state)}
                  (:last-evidence-id state))
              final-state (cond-> (assoc new-state
                                         :last-evidence-id (:evidence/id ev))
                            emitted? (update :events-emitted inc))
              append-err (common/maybe-append-evidence! final-state ev)]
          (if append-err
            append-err
            {:ok true
             :state final-state
             :result {:accepted true
                      :tool tool
                      :mode (:mode final-state)
                      :emitted? emitted?
                      :cursor (cursor-payload final-state)}
             :evidence ev}))))))

(defrecord EmacsCursorPeripheral [spec backend]
  runner/PeripheralRunner

  (start [_ context]
    (if-let [err (runner/validate-context :emacs-cursor context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :emacs-cursor sid author)
            state {:session-id sid
                   :author author
                   :agent-id (when (string? (:agent-id context))
                               (:agent-id context))
                   :editor-id (or (:editor-id context) sid)
                   :mode :follow
                   :focus nil
                   :latest-context nil
                   :latest-minibuffer nil
                   :last-buffer nil
                   :steps []
                   :updates-processed 0
                   :events-emitted 0
                   :last-evidence-id (:evidence/id ev)
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          (do
            (report-surface-projection! state)
            {:ok true :state state :evidence ev})))))

  (step [_ state action]
    (dispatch-step spec state action))

  (stop [_ state reason]
    (let [fruit {:mode (:mode state)
                 :editor-id (:editor-id state)
                 :last-buffer (:last-buffer state)
                 :updates-processed (:updates-processed state)
                 :events-emitted (:events-emitted state)
                 :focus (:focus state)
                 :buffer-surface (buffer-surface state)
                 :minibuffer-surface (minibuffer-surface state)
                 :latest-context (:latest-context state)}
          ev (evidence/make-stop-evidence
              :emacs-cursor
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        (do
          (when-let [agent-id (:agent-id state)]
            (registry/clear-surface-projection! agent-id (projection-source state)))
          {:ok true
           :context {:session-id (:session-id state)
                     :agent-id (:agent-id state)
                     :editor-id (:editor-id state)}
           :fruit fruit
           :evidence ev})))))

(defn make-emacs-cursor
  "Create an emacs-cursor peripheral.
   Backend is accepted for registry compatibility but unused."
  ([] (make-emacs-cursor (tools/make-mock-backend)))
  ([backend]
   (->EmacsCursorPeripheral (common/load-spec :emacs-cursor) backend))
  ([spec backend]
   (->EmacsCursorPeripheral spec backend)))
