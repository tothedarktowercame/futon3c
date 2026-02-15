(ns futon3c.peripheral.discipline
  "Discipline peripheral implementation (PSR/PUR mesh operations).

   Constraints: discipline tools for pattern search/selection and update records.
   Fruit: {:selected-pattern ... :records [...] :summary ...}
   Exit context: {:session-id ... :pattern-id ...}."
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.util UUID]))

(def ^:private psr-tools
  #{:psr-search :psr-select})

(def ^:private pattern-tools
  #{:psr-select :pur-update :pur-mark-pivot})

(def ^:private tool->evidence-type
  {:psr-search :pattern-selection
   :psr-select :pattern-selection
   :pur-update :pattern-outcome
   :pur-mark-pivot :correction
   :par-punctuate :reflection})

(def ^:private tool->claim-type
  {:psr-search :question
   :psr-select :goal
   :pur-update :evidence
   :pur-mark-pivot :correction
   :par-punctuate :conclusion})

(defn- now-str []
  (str (Instant/now)))

(defn- gen-id []
  (str "e-" (UUID/randomUUID)))

(defn- normalize-pattern-id
  [x]
  (cond
    (keyword? x) x
    (string? x)
    (let [s (str/trim x)
          s (if (str/starts-with? s ":") (subs s 1) s)]
      (when-not (str/blank? s)
        (keyword s)))
    :else nil))

(defn- pattern-id->ref-id
  [pattern-id]
  (when pattern-id
    (subs (str pattern-id) 1)))

(defn- resolve-pattern-id
  [tool args result prior]
  (if (pattern-tools tool)
    (or (normalize-pattern-id (:pattern-id result))
        (normalize-pattern-id (:pattern result))
        (normalize-pattern-id (first args))
        prior)
    prior))

(defn- selected-pattern-from
  [tool args result prior]
  (resolve-pattern-id tool args result prior))

(defn- make-step-evidence
  [state tool args result selected-pattern]
  (let [pattern-id (when (pattern-tools tool) selected-pattern)
        pattern-subject? (and pattern-id (pattern-tools tool))
        subject (if pattern-subject?
                  {:ref/type :pattern
                   :ref/id (pattern-id->ref-id pattern-id)}
                  {:ref/type :session
                   :ref/id (:session-id state)})
        claim-type (get tool->claim-type tool :step)
        evidence-type (get tool->evidence-type tool :coordination)
        body (cond-> {:peripheral :discipline
                      :event :step
                      :tool tool
                      :args (vec args)
                      :result result}
               pattern-id (assoc :pattern-id pattern-id))]
    (cond-> {:evidence/id (gen-id)
             :evidence/subject subject
             :evidence/type evidence-type
             :evidence/claim-type claim-type
             :evidence/author (:author state)
             :evidence/at (now-str)
             :evidence/body body
             :evidence/tags (vec (remove nil?
                                         [:peripheral
                                          :discipline
                                          (if (psr-tools tool) :psr :pur)
                                          tool]))
             :evidence/session-id (:session-id state)}
      pattern-id (assoc :evidence/pattern-id pattern-id)
      (:last-evidence-id state) (assoc :evidence/in-reply-to (:last-evidence-id state)))))

(defn- dispatch-step
  [spec backend state action]
  (if-let [err (common/validate-action :discipline action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          dispatch-result (tools/dispatch-tool tool args spec backend)]
      (cond
        (common/social-error? dispatch-result)
        dispatch-result

        (not (:ok dispatch-result))
        (runner/runner-error :discipline :tool-execution-failed
                             "Discipline tool execution failed"
                             :tool tool
                             :args args
                             :result dispatch-result)

        :else
        (let [result (:result dispatch-result)
              selected-pattern (selected-pattern-from
                                 tool args result (:selected-pattern state))
              record {:kind (if (psr-tools tool) :psr :pur)
                      :tool tool
                      :pattern-id selected-pattern
                      :args args
                      :result result}
              ev (make-step-evidence state tool args result selected-pattern)
              new-state (-> state
                            (assoc :last-evidence-id (:evidence/id ev))
                            (assoc :selected-pattern selected-pattern)
                            (update :records conj record))
              append-err (common/maybe-append-evidence! new-state ev)]
          (if append-err
            append-err
            {:ok true
             :state new-state
             :result result
             :evidence ev}))))))

(defrecord DisciplinePeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :discipline context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            initial-pattern (normalize-pattern-id (:pattern-id context))
            ev (evidence/make-start-evidence :discipline sid author)
            state {:session-id sid
                   :author author
                   :selected-pattern initial-pattern
                   :records []
                   :last-evidence-id (:evidence/id ev)
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [records (:records state)
          selected (:selected-pattern state)
          psr-count (count (filter #(= :psr (:kind %)) records))
          pur-count (count (filter #(= :pur (:kind %)) records))
          summary (str "Discipline session complete: "
                       psr-count " PSR op(s), "
                       pur-count " PUR/PAR op(s)")
          fruit {:selected-pattern selected
                 :records records
                 :summary summary
                 :reason (or reason "")}
          ev (evidence/make-stop-evidence
              :discipline
              (:session-id state)
              (:author state)
              fruit
              (or reason "")
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context (cond-> {:session-id (:session-id state)}
                    (keyword? selected)
                    (assoc :pattern-id selected))
         :fruit fruit
         :evidence ev}))))

(defn make-discipline
  "Create a discipline peripheral with injected backend."
  ([] (make-discipline (tools/make-mock-backend)))
  ([backend]
   (->DisciplinePeripheral (common/load-spec :discipline) backend))
  ([spec backend]
   (->DisciplinePeripheral spec backend)))
