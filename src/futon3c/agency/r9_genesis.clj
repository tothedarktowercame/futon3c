(ns futon3c.agency.r9-genesis
  "Non-integrated verification boundary for R9 genesis and successors.

  Trust resolvers are supplied by the host boundary, never selected by the
  candidate anchor. This namespace constructs no anchor and admits nothing."
  (:require [clojure.string :as str]
            [futon2.aif.r9-checker :as r9]))

(def genesis-schema :wm/r9-genesis-candidate-v1)
(def verification-schema :wm/r9-genesis-verification-v1)

(defn- refuse! [cause & [data]]
  (throw (ex-info (name cause) (merge {:refusal cause} data))))

(defn- resolved! [resolver reference cause expected-schema]
  (when-not (fn? resolver) (refuse! cause {:reason :resolver-missing}))
  (let [result (resolver reference)]
    (when-not (and (= :verified (:status result))
                   (= expected-schema (:authority/schema result))
                   (map? (:provenance result))
                   (some? (:authority-origin result)))
      (refuse! cause {:reason (or (:reason result) :not-verified)
                      :resolution (dissoc result :content)}))
    (when (or (= :candidate (:authority-origin result))
              (true? (:candidate-controlled? result)))
      (refuse! :r9/candidate-authored-authority))
    result))

(defn- commission! [resolver expected-job expected-agent]
  (let [resolution (resolved! resolver expected-job :r9/commission-authority-unverified
                              :wm/invoke-commission-resolution-v1)
        envelope (:envelope resolution)]
  (when-not (and (= :agency/invoke-request-commission-v1 (:schema envelope))
                 (= expected-job (:resolved-job-id resolution))
                 (= expected-job (:job-id envelope))
                 (= expected-agent (get-in envelope [:commission :agent-id]))
                 (= expected-agent (get-in envelope [:job-join :agent-id]))
                 (= (get-in envelope [:commission :caller])
                    (get-in envelope [:job-join :caller]))
                 (= (get-in envelope [:commission :surface])
                    (get-in envelope [:job-join :surface]))
                 (map? (:job-join envelope))
                 (= (:request-digest envelope)
                    (r9/request-digest (:commission envelope))))
    (refuse! :r9/commission-join-mismatch {:job-id expected-job}))
  envelope))

(def mandatory-artifact-roles #{:source :tests :review})
(def sha256-pattern #"[0-9a-f]{64}")

(defn- nonblank [x] (and (string? x) (not (str/blank? x))))

(defn- acceptance-subject
  [root author-job-id reviewer-job-id author-c reviewer-c artifacts]
  {:external-root (:authority-root-id root)
   :jobs {:author author-job-id :reviewer reviewer-job-id}
   :commissions {:author (:request-digest author-c)
                 :reviewer (:request-digest reviewer-c)}
   :artifacts (into {} (map (fn [[role pin]] [role (select-keys pin [:id :sha256])])) artifacts)})

(defn verify-candidate
  "Verify against host-injected resolvers. A :verified result is a fixture or
  production verification according to :verification-scope; it is never an anchor."
  [{:keys [candidate root-resolver commission-resolver trace-resolver artifact-resolver acceptance-resolver
           predecessor-resolver]}]
  (when-not (= genesis-schema (:schema candidate))
    (refuse! :r9/genesis-candidate-malformed))
  (let [{:keys [kind author reviewer author-job-id reviewer-job-id
                author-trace-id reviewer-trace-id external-root
                acceptance artifacts
                predecessor]} candidate]
    (when-not (and (nonblank author) (nonblank reviewer))
      (refuse! :r9/role-identity-missing))
    (when (= author reviewer) (refuse! :r9/author-equals-reviewer))
    (when-not (and (nonblank author-job-id) (nonblank reviewer-job-id))
      (refuse! :r9/job-identity-missing))
    (when (= author-job-id reviewer-job-id) (refuse! :r9/author-reviewer-job-equal))
    (when-not (= mandatory-artifact-roles (set (keys artifacts)))
      (refuse! :r9/mandatory-artifacts-missing
               {:required mandatory-artifact-roles :observed (set (keys artifacts))}))
    (doseq [[role pin] artifacts]
      (when-not (and (nonblank (:id pin))
                     (string? (:sha256 pin))
                     (re-matches sha256-pattern (:sha256 pin)))
        (refuse! :r9/artifact-pin-malformed {:artifact role})))
    (let [root (resolved! root-resolver external-root :r9/external-root-unverified
                          :wm/external-root-resolution-v1)
          author-c (commission! commission-resolver author-job-id author)
          reviewer-c (commission! commission-resolver reviewer-job-id reviewer)
          subject (acceptance-subject root author-job-id reviewer-job-id
                                      author-c reviewer-c artifacts)
          acceptance* (resolved! acceptance-resolver acceptance
                                  :r9/delegated-acceptance-unverified
                                  :wm/delegated-acceptance-resolution-v1)
          author-trace (resolved! trace-resolver author-trace-id
                                  :r9/trace-authority-unverified
                                  :wm/trace-resolution-v1)
          reviewer-trace (resolved! trace-resolver reviewer-trace-id
                                    :r9/trace-authority-unverified
                                    :wm/trace-resolution-v1)]
      (when-not (and (= author-job-id (:job-id author-trace))
                     (= reviewer-job-id (:job-id reviewer-trace))
                     (= author-trace-id (get-in author-c [:job-join :trace-id]))
                     (= reviewer-trace-id (get-in reviewer-c [:job-join :trace-id])))
        (refuse! :r9/trace-job-join-mismatch))
      (when-not (and (= :delegated-canonical-branch-acceptance-v1
                        (:schema acceptance*))
                     (= :delegated-technical-lead (:authority acceptance*))
                     (= (:delegate root) (:accepted-by acceptance*))
                     (= "main" (:branch acceptance*))
                     (= reviewer-job-id (:reviewer-job-id acceptance*))
                     (= :accepted (:review-outcome acceptance*))
                     (= subject (:subject acceptance*)))
        (refuse! :r9/delegated-acceptance-unverified))
      (doseq [[role pin] artifacts]
        (let [resolved (resolved! artifact-resolver pin
                                  :r9/artifact-pin-unverified
                                  :wm/artifact-byte-resolution-v1)]
          (when-not (and (= (:id pin) (:artifact-id resolved))
                         (= (:sha256 pin) (:sha256 resolved)))
            (refuse! :r9/artifact-pin-mismatch {:artifact role}))))
      (case kind
        :genesis (when predecessor
                   (refuse! :r9/genesis-has-predecessor))
        :successor (do
                     (when-not (map? predecessor)
                       (refuse! :r9/predecessor-unverified
                                {:reason :predecessor-absent}))
                     (let [prior (resolved! predecessor-resolver predecessor
                                         :r9/predecessor-unverified
                                         :wm/anchored-checker-resolution-v1)]
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
       :verification-scope (:verification-scope root)
       :artifacts (into {} (map (fn [[k v]] [k (:sha256 v)])) artifacts)
       :not-an-anchor true})))

(defn unresolved-host-event-stub
  "Refusal stub for currently located Codex host event references.
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
