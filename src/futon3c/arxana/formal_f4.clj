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

(defn endpoint [role record]
  {:role role :kind :clusion :target {:clusion/id (:id record)}})

(defn build-plan
  "Build the separate, non-executable D6 stages for an F1 ask-action claim.
  SOURCE-ATTESTATION is provenance; it is not copied into claim identity."
  [source-claim source-attestation]
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
                      (get-in source-claim [:endpoints 0 :target :artifact/revision-id])
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
                           links [composite group]))}))

(defn materialize! [dispatch! plan]
  (mapv (fn [record]
          (dispatch! {:protocol/version "arxana.v1"
                      :message/id (str "f4a:" (:id record))
                      :operation :clusion/assert :principal/id "formal-f4-materialiser"
                      :body {:record record}}))
        (:records plan)))

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
                                                 attestations)]
                  {:port role :record/id rid :type-revision (:type-revision record)
                   :assessment/verdicts (mapv :verdict assessments)
                   :evidence/attestation-ids (mapv :id critical-evidence)
                   :criticised? (boolean (or (seq assessments)
                                             (seq critical-evidence)))}))
              (:endpoints composite))]
    {:rule/id :formal-f4/composite :rule/version 1
     :composite/id id :composite/record composite :stages rows
     :drops [:stage-record-content :assessment-reasons :evidence-content]}))
