(ns futon3c.peripheral.alfworld
  "ALFWorld stepper peripheral implementation.

   Constraints: ALFWorld actions only (go, take, put, open, etc.).
                Can bell Joe or whistle Codex for coordination.
                Can hop out at any time.

   Fruit: {:won boolean, :score float, :final-observation string,
           :step-count int, :coordination-events [...]}

   Exit context: {:session-id ..., :alfworld-state {...}, :bells-sent [...]}

   The 'stepper' aspect: agent can hop in/out of alfworld freely, with state
   persisting between hops. While inside, receives both alfworld observations
   AND coordination bells from Joe/Codex (P-6 interleaved streams)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; ALFWorld HTTP client
;; =============================================================================

(defn- http-post
  "Simple blocking POST to ALFWorld server. Returns parsed JSON or error map."
  [url body]
  (try
    (let [proc (-> (ProcessBuilder. ["curl" "-s" "-X" "POST"
                                     "-H" "Content-Type: application/json"
                                     "-d" (json/generate-string body)
                                     url])
                   (.start))
          exit (.waitFor proc)
          out (slurp (.getInputStream proc))]
      (if (zero? exit)
        (json/parse-string out true)
        {:error (str "curl failed with exit " exit)}))
    (catch Exception e
      {:error (str "HTTP POST failed: " (.getMessage e))})))

(defn- http-get
  "Simple blocking GET from ALFWorld server. Returns parsed JSON or error map."
  [url]
  (try
    (let [proc (-> (ProcessBuilder. ["curl" "-s" url])
                   (.start))
          exit (.waitFor proc)
          out (slurp (.getInputStream proc))]
      (if (zero? exit)
        (json/parse-string out true)
        {:error (str "curl failed with exit " exit)}))
    (catch Exception e
      {:error (str "HTTP GET failed: " (.getMessage e))})))

(defn- alfworld-reset!
  "Reset the ALFWorld environment to a new random task."
  [server-url]
  (http-post (str server-url "/reset") {}))

(defn- alfworld-step!
  "Take an action in ALFWorld. Returns new state."
  [server-url action]
  (http-post (str server-url "/step") {:action action}))

(defn- alfworld-state
  "Get current ALFWorld state without taking an action."
  [server-url]
  (http-get (str server-url "/state")))

;; =============================================================================
;; Coordination primitives (bell/whistle)
;; =============================================================================

(defn- bell!
  "Send a bell notification to another agent or Joe.
   Appends to :bells-sent in state for evidence trail.
   Returns updated state."
  [state target message]
  (let [bell {:timestamp (str (java.time.Instant/now))
              :from "alfworld-agent"
              :to target
              :message message
              :type :bell}]
    (println (format "[BELL] alfworld → %s: %s" target message))
    (flush)
    (update state :bells-sent conj bell)))

(defn- whistle!
  "Send a whistle (blocking request) to another agent.
   For now, this is a simulation — just records the request.
   Real implementation would call Codex via WS and wait for response."
  [state target message]
  (let [whistle {:timestamp (str (java.time.Instant/now))
                 :from "alfworld-agent"
                 :to target
                 :message message
                 :type :whistle
                 :response "TODO: implement real whistle via WS"}]
    (println (format "[WHISTLE] alfworld → %s: %s" target message))
    (println "         (waiting for response... [simulated])")
    (flush)
    (update state :whistles-sent conj whistle)))

;; =============================================================================
;; Action dispatch
;; =============================================================================

(def ^:private alfworld-actions
  "Set of valid ALFWorld action verbs."
  #{"go" "take" "put" "open" "close" "toggle" "clean" "heat"
    "cool" "use" "examine" "inventory" "look"})

(defn- alfworld-action?
  "Check if action string is a valid ALFWorld command."
  [action]
  (when (string? action)
    (let [verb (first (str/split (str/trim action) #"\s+"))]
      (contains? alfworld-actions (str/lower-case verb)))))

(defn- dispatch-alfworld-action
  "Execute an ALFWorld action (go/take/put/etc.) and update state."
  [state action server-url]
  (let [result (alfworld-step! server-url action)]
    (if (:error result)
      (runner/runner-error :alfworld :alfworld-step-failed
                           (:error result)
                           :action action
                           :result result)
      (let [ev (evidence/make-step-evidence
                :alfworld
                (:session-id state)
                (:author state)
                :alfworld-step
                {:action action}
                {:observation (:observation result)
                 :done (:done result)
                 :won (:won result)
                 :score (:score result)
                 :step (:step result)}
                (:last-evidence-id state))
            new-state (-> state
                          (assoc :last-evidence-id (:evidence/id ev))
                          (assoc :alfworld-state result)
                          (update :step-count inc)
                          (update :actions-taken conj {:action action :result result}))
            append-err (common/maybe-append-evidence! new-state ev)]
        (if append-err
          append-err
          {:ok true
           :state new-state
           :result result
           :evidence ev})))))

(defn- dispatch-coordination-action
  "Handle bell/whistle/hop coordination actions."
  [state action]
  (let [{:keys [coord-type target message]} action]
    (case coord-type
      :bell
      (let [new-state (bell! state target message)]
        {:ok true
         :state new-state
         :result {:coord-type :bell :sent true}
         :evidence nil})

      :whistle
      (let [new-state (whistle! state target message)]
        {:ok true
         :state new-state
         :result {:coord-type :whistle :sent true}
         :evidence nil})

      :hop-out
      ;; Signal intent to exit peripheral (actual hop happens via stop)
      {:ok true
       :state (assoc state :hop-out-requested true :hop-out-reason message)
       :result {:coord-type :hop-out :reason message}
       :evidence nil}

      (runner/runner-error :alfworld :unknown-coordination-action
                           (str "Unknown coordination type: " coord-type)
                           :action action))))

(defn- dispatch-step
  "Dispatch a single action: either alfworld command or coordination."
  [spec backend state action]
  (let [server-url (or (:alfworld-server spec) "http://localhost:3456")]
    (cond
      ;; Coordination action (bell/whistle/hop)
      (map? action)
      (dispatch-coordination-action state action)

      ;; ALFWorld action (string command)
      (string? action)
      (if (alfworld-action? action)
        (dispatch-alfworld-action state action server-url)
        (runner/runner-error :alfworld :invalid-action
                             (str "Not a valid ALFWorld action: " action)
                             :action action
                             :valid-verbs alfworld-actions))

      :else
      (runner/runner-error :alfworld :malformed-action
                           "Action must be string (alfworld cmd) or map (coordination)"
                           :action action))))

;; =============================================================================
;; PeripheralRunner implementation
;; =============================================================================

(defrecord ALFWorldPeripheral [spec backend]
  runner/PeripheralRunner

  (start [_ context]
    (if-let [err (runner/validate-context :alfworld context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            server-url (or (:alfworld-server spec) "http://localhost:3456")
            ;; Check if we're resuming existing state or starting fresh
            existing-state (:alfworld-state context)
            initial-state (if existing-state
                            existing-state
                            (alfworld-reset! server-url))]
        (if (:error initial-state)
          (runner/runner-error :alfworld :start-failed
                               (:error initial-state)
                               :server-url server-url)
          (let [ev (evidence/make-start-evidence :alfworld sid author)
                state {:session-id sid
                       :author author
                       :last-evidence-id (:evidence/id ev)
                       :alfworld-state initial-state
                       :step-count 0
                       :actions-taken []
                       :bells-sent []
                       :whistles-sent []
                       :hop-out-requested false
                       :hop-out-reason nil
                       :evidence-store (:evidence-store context)}
                append-err (common/maybe-append-evidence! state ev)]
            (if append-err
              append-err
              {:ok true :state state :evidence ev}))))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [alf-state (:alfworld-state state)
          fruit {:won (:won alf-state)
                 :score (:score alf-state)
                 :final-observation (:observation alf-state)
                 :step-count (:step-count state)
                 :coordination-events {:bells-sent (:bells-sent state)
                                       :whistles-sent (:whistles-sent state)}}
          ;; Include alfworld state in exit context for hop continuity
          exit-context (cond-> {:session-id (:session-id state)
                                :alfworld-state alf-state
                                :bells-sent (:bells-sent state)}
                         (:hop-out-requested state)
                         (assoc :hop-out-reason (:hop-out-reason state)))
          ev (evidence/make-stop-evidence
              :alfworld
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :fruit fruit
         :context exit-context
         :evidence ev}))))

(defn make-alfworld
  "Factory function for ALFWorld peripheral.
   spec: {:alfworld-server \"http://localhost:3456\" ...}
   backend: ToolBackend (unused for alfworld, but required by protocol)"
  ([backend]
   (make-alfworld (common/load-spec :alfworld) backend))
  ([spec backend]
   (->ALFWorldPeripheral spec backend)))
