(ns futon3c.social.validate
  "S-validate: validate coordination outcomes against accumulated evidence.

   Uses the evidence store as the source of truth (R8) and structured events (R9).
   Returns CoordinationOutcome on success, SocialError on failure."
  (:require [futon3c.evidence.store :as store]
            [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :E-validate
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- dispatch->msg-id
  [dispatch-result]
  (cond
    (shapes/valid? shapes/DispatchReceipt dispatch-result)
    (:receipt/msg-id dispatch-result)

    (and (map? dispatch-result) (string? (:msg/id dispatch-result)))
    (:msg/id dispatch-result)

    :else nil))

(defn validate-outcome
  "Validate a coordination outcome against accumulated evidence and patterns.
   Returns CoordinationOutcome on success, SocialError on failure."
  [dispatch-result patterns evidence-store]
  (cond
    (not (shapes/valid? shapes/PatternLibrary patterns))
    (social-error :invalid-patterns
                  "Invalid pattern library input"
                  :patterns patterns
                  :validation (or (:error (shapes/validate shapes/PatternLibrary patterns)) {}))

    (nil? evidence-store)
    (social-error :missing-store "Evidence store is required")

    :else
    (let [msg-id (dispatch->msg-id dispatch-result)]
      (if-not (string? msg-id)
        (social-error :invalid-dispatch-result
                      "dispatch-result must be a DispatchReceipt or map with :msg/id"
                      :dispatch-result dispatch-result)
        (let [subject {:ref/type :evidence :ref/id msg-id}
              entries (store/query* evidence-store {:query/subject subject})
              entries (cond
                        (vector? entries) entries
                        (sequential? entries) (vec entries)
                        :else [])]
          (if (empty? entries)
            (social-error :missing-evidence
                          "No evidence found for dispatch result"
                          :msg-id msg-id
                          :subject subject)
            (let [has-conclusion? (some #(= :conclusion (:evidence/claim-type %)) entries)
                  by-pattern (->> entries (keep :evidence/pattern-id) frequencies)
                  outcome {:outcome/id (str "out-" (UUID/randomUUID))
                           :outcome/type (if has-conclusion?
                                           :coordination-complete
                                           :coordination-failed)
                           :outcome/valid? (boolean has-conclusion?)
                           :outcome/evidence {:msg-id msg-id
                                              :subject subject
                                              :entry-count (count entries)
                                              :has-conclusion? (boolean has-conclusion?)
                                              :patterns/applied by-pattern
                                              :patterns/available (:patterns/ids patterns)}
                           :outcome/at (now-str)}]
              (if (shapes/valid? shapes/CoordinationOutcome outcome)
                outcome
                (social-error :invalid-outcome
                              "Internal error: CoordinationOutcome did not conform to shape"
                              :outcome outcome
                              :validation (or (:error (shapes/validate shapes/CoordinationOutcome outcome)) {}))))))))))
