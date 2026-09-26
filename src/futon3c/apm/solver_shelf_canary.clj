(ns futon3c.apm.solver-shelf-canary
  "Fail-closed authority for a preregistered Solver memory-shelf canary."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]))

(def assignments #{:shelf :control})
(defn shelf-digest [entries] (machine/ledger-digest [entries]))

(defn validate-assignment [assignment frame-id]
  (let [entries (:shelf/entries assignment)
        ids (mapv :memory-id entries)
        kind (:assignment assignment)
        findings (cond-> []
                   (not= 1 (:schema/version assignment)) (conj :schema-version-invalid)
                   (not (and (string? (:canary/id assignment))
                             (not (str/blank? (:canary/id assignment))))) (conj :canary-id-invalid)
                   (not= frame-id (:eligible/frame-id assignment)) (conj :frame-not-eligible)
                   (not (contains? assignments kind)) (conj :assignment-invalid)
                   (not (nat-int? (:matched/size assignment))) (conj :matched-size-invalid)
                   (not (vector? entries)) (conj :shelf-entries-invalid)
                   (and (= :shelf kind) (empty? entries)) (conj :shelf-empty)
                   (and (= :control kind) (seq entries)) (conj :control-exposed)
                   (and (vector? entries)
                        (not (every? #(and (string? (:memory-id %))
                                          (string? (:hook %))
                                          (string? (:body %))) entries))) (conj :shelf-entry-invalid)
                   (and (vector? entries) (not= (count ids) (count (distinct ids)))) (conj :shelf-id-duplicate)
                   (and (= :shelf kind) (not= (:matched/size assignment) (count entries))) (conj :matched-size-mismatch)
                   (and (= :control kind) (not (pos? (:matched/size assignment)))) (conj :control-match-size-missing)
                   (and (vector? entries)
                        (not= (:shelf/digest assignment) (shelf-digest entries))) (conj :shelf-digest-mismatch))]
    (if (seq findings)
      {:ok false :error/code :solver-shelf-authority-invalid :findings findings}
      {:ok true :assignment assignment})))

(defn observation-findings [assignment observation]
  (let [allowed (set (map :memory-id (:shelf/entries assignment)))
        surfaced (set (:surfaced-ids observation))
        used (set (:used-ids observation))]
    (cond-> []
      (not (map? observation)) (conj :solver-shelf-observation-missing)
      (not (vector? (:surfaced-ids observation))) (conj :surfaced-ids-invalid)
      (not (vector? (:used-ids observation))) (conj :used-ids-invalid)
      (not (every? string? surfaced)) (conj :surfaced-id-invalid)
      (not (every? string? used)) (conj :used-id-invalid)
      (not= surfaced allowed) (conj :surfaced-ids-authority-mismatch)
      (not (every? surfaced used)) (conj :used-id-not-surfaced))))
