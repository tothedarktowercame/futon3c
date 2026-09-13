(ns futon3c.agency.r9-genesis
  "Non-integrated verification boundary for R9 genesis and successors.

  Trust resolvers are supplied by the host boundary, never selected by the
  candidate anchor. This namespace constructs no anchor and admits nothing."
  (:require [futon2.aif.r9-checker :as r9]))

(def genesis-schema :wm/r9-genesis-candidate-v1)
(def verification-schema :wm/r9-genesis-verification-v1)

(defn- refuse! [cause & [data]]
  (throw (ex-info (name cause) (merge {:refusal cause} data))))

(defn- resolved! [resolver reference cause]
  (when-not (fn? resolver) (refuse! cause {:reason :resolver-missing}))
  (let [result (resolver reference)]
    (when-not (= :verified (:status result))
      (refuse! cause {:reason (or (:reason result) :not-verified)
                      :resolution (dissoc result :content)}))
    (when (or (= :candidate (:authority-origin result))
              (true? (:candidate-controlled? result)))
      (refuse! :r9/candidate-authored-authority))
    result))

(defn- commission! [envelope expected-job expected-agent]
  (when-not (and (= :agency/invoke-request-commission-v1 (:schema envelope))
                 (= expected-job (:job-id envelope))
                 (= expected-agent (get-in envelope [:commission :agent-id]))
                 (= (:request-digest envelope)
                    (r9/request-digest (:commission envelope))))
    (refuse! :r9/commission-join-mismatch {:job-id expected-job}))
  envelope)

(defn verify-candidate
  "Verify a candidate against independently injected authorities.

  ROOT-RESOLVER authenticates the operator/delegation event, TRACE-RESOLVER
  maps trace ids to jobs, ARTIFACT-RESOLVER returns verified sha256 pins, and
  PREDECESSOR-RESOLVER verifies a previously anchored checker for successors.
  A successful return is verification evidence only, never an anchor."
  [{:keys [candidate root-resolver trace-resolver artifact-resolver acceptance-resolver
           predecessor-resolver]}]
  (when-not (= genesis-schema (:schema candidate))
    (refuse! :r9/genesis-candidate-malformed))
  (let [{:keys [kind author reviewer author-job-id reviewer-job-id
                author-trace-id reviewer-trace-id external-root
                author-commission reviewer-commission acceptance artifacts
                predecessor]} candidate]
    (when (= author reviewer) (refuse! :r9/author-equals-reviewer))
    (let [root (resolved! root-resolver external-root :r9/external-root-unverified)
          author-c (commission! author-commission author-job-id author)
          reviewer-c (commission! reviewer-commission reviewer-job-id reviewer)
          acceptance* (resolved! acceptance-resolver acceptance
                                  :r9/delegated-acceptance-unverified)
          author-trace (resolved! trace-resolver author-trace-id
                                  :r9/trace-authority-unverified)
          reviewer-trace (resolved! trace-resolver reviewer-trace-id
                                    :r9/trace-authority-unverified)]
      (when-not (and (= author-job-id (:job-id author-trace))
                     (= reviewer-job-id (:job-id reviewer-trace)))
        (refuse! :r9/trace-job-join-mismatch))
      (when-not (and (= :delegated-canonical-branch-acceptance-v1
                        (:schema acceptance*))
                     (= :delegated-technical-lead (:authority acceptance*))
                     (= (:delegate root) (:accepted-by acceptance*))
                     (= "main" (:branch acceptance*))
                     (= reviewer-job-id (:reviewer-job-id acceptance*)))
        (refuse! :r9/delegated-acceptance-unverified))
      (doseq [[role pin] artifacts]
        (let [resolved (resolved! artifact-resolver pin
                                  :r9/artifact-pin-unverified)]
          (when-not (= (:sha256 pin) (:sha256 resolved))
            (refuse! :r9/artifact-pin-mismatch {:artifact role}))))
      (case kind
        :genesis (when predecessor
                   (refuse! :r9/genesis-has-predecessor))
        :successor (do
                     (when-not (map? predecessor)
                       (refuse! :r9/predecessor-unverified
                                {:reason :predecessor-absent}))
                     (let [prior (resolved! predecessor-resolver predecessor
                                         :r9/predecessor-unverified)]
                     (when-not (= (:checker-source-sha256 predecessor)
                                  (:checker-source-sha256 prior))
                       (refuse! :r9/predecessor-pin-mismatch))))
        (refuse! :r9/genesis-kind-invalid))
      {:schema verification-schema
       :decision :verified-for-independent-review
       :kind kind
       :roles {:author author :reviewer reviewer}
       :jobs {:author author-job-id :reviewer reviewer-job-id}
       :commissions {:author (:request-digest author-c)
                     :reviewer (:request-digest reviewer-c)}
       :traces {:author author-trace-id :reviewer reviewer-trace-id}
       :external-root (:authority-root-id root)
       :acceptance-digest (:digest acceptance*)
       :artifacts (into {} (map (fn [[k v]] [k (:sha256 v)])) artifacts)
       :not-an-anchor true})))

(defn located-host-event-resolver
  "Read-only resolver for the currently located Codex host event references.
  The host JSONL is user-owned and mutable and supplies no authenticated human
  origin, so a matching record returns located-not-authenticated, never verified."
  [reference]
  {:status :located-not-authenticated
   :reason :host-user-role-record-has-no-independent-origin-authentication
   :authority-origin :host-session-log
   :source (:source reference)
   :line (:line reference)
   :record-sha256 (:record-sha256-including-newline reference)
   :ownership {:expected-user "joe"
               :administrator "joe/root"
               :mutable? true
               :cryptographic-operator-signature? false}})
