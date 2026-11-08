(ns futon3c.arxana.formal-f4
  "F4a: inert D6 stages and recursively addressable typed composites."
  (:require [futon3c.arxana.compile.admission :as admission]
            [futon3c.arxana.compile.compiler :as compiler]
            [futon3c.arxana.compile.intent :as intent]
            [futon3c.arxana.records :as records]))

(def topology-evidence
  {:path "holes/labs/M-formal-arxana/f4-d6-composite.edn"
   :sha256 "df69f83fa434bdc0decb523662f29d562f10d647a116b4e5093f2b8ef8cfffc8"})

(def dynamics {:id "formal-arxana/d6-compose" :version 1})
(def bounded-read-dynamics {:id "formal-arxana/bounded-read" :version 1})
(declare ok-record)

(defn endpoint [role record]
  {:role role :kind :clusion :target {:clusion/id (:id record)}})

(defn build-plan
  "Build the separate, non-executable D6 stages for an F1 ask-action claim.
  SOURCE-ATTESTATION is provenance; it is not copied into claim identity.
  GROUNDING-REVISION is the turn's stored byte revision: the claim names a
  logical `<turn>@settled` id, which is never an artifact head, so the
  candidate's head-equals precondition must name the stored revision."
  ([source-claim source-attestation]
   (build-plan source-claim source-attestation nil))
  ([source-claim source-attestation grounding-revision]
  (let [intent-record
        (intent/make-intent
         {:intent/type :action/request
          :intent/source-act-id (:id source-attestation)
          :intent/target-ref (get-in source-claim [:endpoints 2 :target])
          :intent/proposed-change {:request "send the remaining MAP work"}
          :interpreter {:interpreter/id (:producer source-attestation)
                        :interpreter/type :model
                        :interpreter/model "象-sonnet"
                        :interpreter/model-version "fixture-v1"}
          :evidence [(:id source-attestation)]})
        compiled
        (compiler/compile-action
         {:source-act/id (:id source-attestation)
          :intent intent-record
          :trust-profile (intent/make-trust-profile
                          {:trust-profile/id "formal-f4/f1-fixture"
                           :trust-profile/version 1
                           :trusted-interpreters {(:producer source-attestation) true}})
          :grounding {:artifact/revision-id
                      (or grounding-revision
                          (get-in source-claim [:endpoints 0 :target :artifact/revision-id]))
                      :resolution-assertion {:resolution-assertion/id
                                             (str "formal-f4:resolution:" (:id source-claim))
                                             :evidence/outcome :unique}
                      :correspondence-assertions []
                      :grounding/predicted-effects [{:effect :delegate-work}]
                      :grounding/risks []}
          :operation/overrides {:type :agency/request
                                :target (:intent/target-ref intent-record)}})
        candidate (:candidate-action compiled)
        decision (admission/make-admission-decision
                  {:decision/status :deferred
                   :policy/id "formal-f4/no-execution"
                   :policy/revision 1 :actor "formal-f4-fixture"
                   :reasons ["F4a constructs records but does not execute"]
                   :conditions [] :candidate-action/id (:candidate-action/id candidate)})
        intent-stage (records/make-clusion
                      {:type-revision [:d6/intent-stage 1]
                       :endpoints [(endpoint :source source-claim)]
                       :asserted-content {:intent/id (:intent/id intent-record)
                                          :intent/record intent-record}})
        candidate-stage (records/make-clusion
                         {:type-revision [:d6/candidate-stage 1]
                          :endpoints [(endpoint :intent intent-stage)]
                          :asserted-content
                          {:candidate/id (:candidate-action/id candidate)
                           :candidate/record candidate
                           :candidate/non-executable? (compiler/non-executable? candidate)}})
        decision-stage (records/make-clusion
                        {:type-revision [:d6/decision-stage 1]
                         :endpoints [(endpoint :candidate candidate-stage)]
                         :asserted-content {:decision/id (:admission/id decision)
                                            :decision/record decision}})
        link (fn [tr endpoints]
               (records/make-clusion {:type-revision tr :endpoints endpoints
                                      :asserted-content {}}))
        links [(link [:d6/intends 1]
                     [(endpoint :source-act source-claim) (endpoint :intent intent-stage)])
               (link [:d6/compiles-to 1]
                     [(endpoint :intent intent-stage) (endpoint :candidate candidate-stage)])
               (link [:d6/decides 1]
                     [(endpoint :candidate candidate-stage) (endpoint :decision decision-stage)])]
        composite (records/make-clusion
                   {:type-revision [:d6/composite 1]
                    :endpoints [(endpoint :source-act source-claim)
                                (endpoint :intent intent-stage)
                                (endpoint :candidate candidate-stage)
                                (endpoint :decision decision-stage)]
                    :asserted-content {:dynamics dynamics
                                       :topology/evidence topology-evidence}})
        group (records/make-clusion
               {:type-revision [:d6/composite-group 1]
                :endpoints [(endpoint :child composite)]
                :asserted-content {:dynamics dynamics
                                   :topology/evidence topology-evidence}})]
    {:source source-claim :intent intent-record :candidate candidate :decision decision
     :stages {:intent intent-stage :candidate candidate-stage :decision decision-stage}
     :links links :composite composite :group group
     :records (vec (concat [intent-stage candidate-stage decision-stage]
                           links [composite group]))})))

(defn materialize! [dispatch! plan]
  (mapv (fn [record]
          (dispatch! {:protocol/version "arxana.v1"
                      :message/id (str "f4a:" (:id record))
                      :operation :clusion/assert :principal/id "formal-f4-materialiser"
                      :body {:record record}}))
        (:records plan)))

(defn capability
  [candidate-stage principal actor granted-at]
  (records/make-clusion
   {:type-revision [:d6/capability 1]
    :endpoints [(endpoint :candidate candidate-stage)]
    :asserted-content {:principal/id principal :dynamics bounded-read-dynamics
                       :granted-by actor :granted-at granted-at}}))

(defn admitted-decision [candidate-stage candidate]
  (let [decision (admission/make-admission-decision
                  {:decision/status :admitted
                   :policy/id "formal-f4/bounded-read-policy" :policy/revision 1
                   :actor "formal-f4-admission" :reasons ["bounded read-only query"]
                   :conditions [{:query/max-limit 10}]
                   :candidate-action/id (:candidate-action/id candidate)})]
    (records/make-clusion
     {:type-revision [:d6/decision-stage 1]
      :endpoints [(endpoint :candidate candidate-stage)]
      :asserted-content {:decision/id (:admission/id decision)
                         :decision/record decision}})))

(defn- record! [write! op record suffix]
  (write! op {:record record} (str suffix ":" (:id record))))

(defn- expected-revision [candidate-stage]
  (some #(when (= :revision/head-equals (:type %)) (:revision/id %))
        (get-in candidate-stage [:asserted-content :candidate/record :preconditions])))

(defn- refusal-code
  [principal candidate-stage capability-record decision-stage expected current-head]
  (let [candidate-id (get-in candidate-stage
                             [:asserted-content :candidate/record :candidate-action/id])
        cap-content (:asserted-content capability-record)
        decision (get-in decision-stage [:asserted-content :decision/record])]
    (cond
      (nil? capability-record) :arxana.execution/capability-missing
      (not= [:d6/capability 1] (:type-revision capability-record))
      :arxana.execution/invalid-capability-record
      (not= principal (:principal/id cap-content)) :arxana.execution/principal-not-capable
      (not= bounded-read-dynamics (:dynamics cap-content)) :arxana.execution/dynamics-not-capable
      (not= (:id candidate-stage)
            (get-in capability-record [:endpoints 0 :target :clusion/id]))
      :arxana.execution/capability-for-different-candidate
      (nil? decision-stage) :arxana.execution/admission-missing
      (not= [:d6/decision-stage 1] (:type-revision decision-stage))
      :arxana.execution/invalid-admission-record
      (not= :admitted (:decision/status decision)) :arxana.execution/not-admitted
      (not= candidate-id (:candidate-action/id decision))
      :arxana.execution/admission-for-different-candidate
      (not= expected (expected-revision candidate-stage)) :arxana.execution/stale-revision
      ;; Tripwire 4: the candidate's revision must still be the artifact's
      ;; head in the store, not merely match what the caller says it is.
      (not= current-head (expected-revision candidate-stage))
      :arxana.execution/stale-revision
      :else nil)))

(defn execute
  "Execute one bounded public read after checking stored capability and
  admission. Every path appends an attempt receipt and a successor composite;
  only success carries the separate D6 action receipt."
  [read! write! current-head! principal request-id
   {composite-id :composite/id capability-id :capability/id
    decision-id :decision/id expected :expected/revision-id
    attempted-at :attempted-at limit :query/limit}]
  (let [composite (ok-record read! composite-id)
        by-role (into {} (map (juxt :role identity) (:endpoints composite)))
        stage-id #(get-in by-role [% :target :clusion/id])
        candidate-stage (ok-record read! (stage-id :candidate))
        capability-record (ok-record read! capability-id)
        decision-stage (ok-record read! decision-id)
        current-head (some-> candidate-stage expected-revision current-head!)
        refusal (refusal-code principal candidate-stage capability-record decision-stage
                              expected current-head)
        envelope {:query/type :role-reverse :edge/type [:d6/intends 1]
                  :endpoint/role :source-act :endpoint/id (stage-id :source-act)
                  :query/limit (min 10 (max 1 (or limit 10)))}
        query-result (when-not refusal (read! :graph/query envelope))
        query-error (:error query-result)
        ids (get-in query-result [:ok :result/edge-ids])
        outcome (cond refusal :refused
                      query-error :failed
                      (seq ids) :ran-with-output
                      :else :ran-no-output)
        reason (or refusal
                   (when query-error :arxana.execution/bounded-read-failed)
                   (if (seq ids) :arxana.execution/bounded-read-returned
                       :arxana.execution/bounded-read-empty))
        attempt (records/make-attempt
                 {:subject/id composite-id :dynamics bounded-read-dynamics
                  :inputs {:envelope envelope :expected/revision-id expected
                   :observed/head-revision-id current-head
                           :capability/id capability-id :decision/id decision-id}
                  :outcome outcome :reason reason :actor principal :time attempted-at})
        attempt-write (record! write! :attempt/append attempt request-id)
        action-receipt
        (when (= :ran-with-output outcome)
          (admission/make-receipt
           {:request/id request-id :operation/id "formal-arxana/bounded-read"
            :candidate-action/id
            (get-in candidate-stage [:asserted-content :candidate/record
                                     :candidate-action/id])
            :admission/id (get-in decision-stage [:asserted-content :decision/id])
            :revision/before expected :revision/after expected
            :actual-effects [{:effect :read :record/ids (vec ids)}]
            :deviations [] :outcome :applied :evidence [(:id attempt)]}))
        receipt-stage (records/make-clusion
                       {:type-revision [:d6/receipt-stage 1]
                        :endpoints [(endpoint :decision decision-stage)]
                        :asserted-content {:attempt/id (:id attempt)
                                           :action/receipt action-receipt}})
        successor (records/make-clusion
                   {:type-revision [:d6/composite 2]
                    :endpoints [(endpoint :predecessor composite)
                                (assoc (by-role :source-act) :role :source-act)
                                (assoc (by-role :intent) :role :intent)
                                (assoc (by-role :candidate) :role :candidate)
                                (endpoint :decision decision-stage)
                                (endpoint :receipt receipt-stage)]
                    :asserted-content {:dynamics dynamics
                                       :topology/evidence topology-evidence}})
        stage-write (record! write! :clusion/assert receipt-stage request-id)
        composite-write (record! write! :clusion/assert successor request-id)]
    {:result/status (if refusal :refused (if query-error :failed :ok))
     :refusal/code refusal :attempt/id (:id attempt) :attempt/outcome outcome
     :action/receipt action-receipt :composite/id (:id successor)
     :result/edge-ids (vec ids)
     :writes {:attempt attempt-write :receipt-stage stage-write
              :composite composite-write}}))

(defn- ok-record [read! id]
  (get-in (read! :entity/get {:entity/id id}) [:ok :record]))

(defn project
  "Derived standing is stage-local. It never rewrites the composite."
  [read! {:keys [composite/id]}]
  (let [composite (ok-record read! id)
        rows (mapv
              (fn [{:keys [role target]}]
                (let [rid (:clusion/id target)
                      record (ok-record read! rid)
                      assessments (get-in (read! :graph/query
                                                 {:query/type :assessments
                                                  :subject/id rid :query/limit 100})
                                          [:ok :result/assessments])
                      attestation-ids (get-in (read! :graph/query
                                                    {:query/type :attestations
                                                     :subject/id rid :query/limit 100})
                                             [:ok :result/attestation-ids])
                      attestations (mapv #(ok-record read! %) attestation-ids)
                      critical-evidence (filterv #(contains? #{:contradicted :disputed}
                                                             (:standing %))
                                                 attestations)
                      attempt-id (get-in record [:asserted-content :attempt/id])
                      attempt (when attempt-id (ok-record read! attempt-id))]
                  (cond-> {:port role :record/id rid :type-revision (:type-revision record)
                           :assessment/verdicts (mapv :verdict assessments)
                           :evidence/attestation-ids (mapv :id critical-evidence)
                           :criticised? (boolean (or (seq assessments)
                                                     (seq critical-evidence)))}
                    attempt (assoc :attempt/id attempt-id
                                   :attempt/outcome (:outcome attempt)))))
              (:endpoints composite))]
    {:rule/id :formal-f4/composite :rule/version 1
     :composite/id id :composite/record composite :stages rows
     :drops [:stage-record-content :assessment-reasons :evidence-content]}))
