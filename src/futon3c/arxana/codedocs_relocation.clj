(ns futon3c.arxana.codedocs-relocation
  "Exact F0 relocation and stored-evidence reporting for the code↔docs slice."
  (:require [futon3c.arxana.formal-f1-relocation :as relocation]
            [futon3c.arxana.service :as service]))

(defn strategy
  "Capture F1's exact anchor for one historical code-definition scope."
  [source-text source-revision-id scope-id codepoint-range]
  (relocation/strategy source-text source-revision-id
                       {:clusion/id scope-id :range codepoint-range}))

(defn relocate-one
  "Resolve one historical scope against an explicit target revision."
  [provider source-text source-revision-id target-revision-id scope-id range]
  (let [s (strategy source-text source-revision-id scope-id range)
        registry {(:strategy/revision-id s) s}]
    (first (:results (relocation/relocate provider registry source-revision-id
                                          target-revision-id target-revision-id
                                          target-revision-id)))))

(defn append-one!
  "Append the resolver result as evidence about the immutable old scope."
  [cfg result]
  (let [attestation (relocation/relocation-attestation result)
        response (service/dispatch
                  cfg {:protocol/version "arxana.v1"
                       :message/id (str "codedocs:relocation:" (:id attestation))
                       :operation :evidence/append
                       :principal/id "codedocs/relocator"
                       :body {:record attestation}})]
    {:attestation attestation :response response}))

(defn outcome
  "Use the packet's vocabulary while preserving F0's :resolved status."
  [stored-attestation]
  (case (get-in stored-attestation [:content :resolution/status])
    :resolved :unique
    :ambiguous :ambiguous
    :missing :missing))
