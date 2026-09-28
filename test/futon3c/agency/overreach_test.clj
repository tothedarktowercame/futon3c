(ns futon3c.agency.overreach-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.overreach :as overreach]))

(def start "2026-09-24T16:00:00Z")
(def until "2026-09-24T17:00:00Z")
(def in-time "2026-09-24T16:30:00Z")

(defn grant
  ([id grantor grantee parent]
   (grant id grantor grantee parent start until [:deploy] ["act:rule"] :explicit))
  ([id grantor grantee parent from to kinds rules basis]
   {:hx/id id
    :hx/type :grant/record
    :hx/props (cond->
                {:grant/grantor grantor
                 :grant/grantee grantee
                 :grant/scope {:description "deploy authority"
                               :act-kinds kinds :rule-ids rules}
                 :grant/interval (cond-> {:from from} to (assoc :until to))
                 :grant/source {:id "e-source" :author grantor :at from :quote "do it"}
                 :grant/basis basis}
                parent (assoc :grant/parent parent))}))

(def root (grant "act:grant-root" "joe" "agent-a" nil))

(defn stamp
  ([executor grant-id] (stamp executor grant-id :session-bound))
  ([executor grant-id basis]
   {:executor executor :signer executor :authority {:grant grant-id}
    :executor-basis basis}))

(defn act
  ([id grant-id] (act id grant-id :deploy in-time "agent-a"))
  ([id grant-id kind at executor]
   {:act/id id :act/kind kind :act/rule-id nil :act/at at
    :act/target-signer executor :act/stamp (stamp executor grant-id)}))

(defn reason-for [acts grants]
  (mapv :finding/reason (overreach/scan acts grants)))

(deftest acceptance-four-cases-report-only-uncovered-acts
  (let [acts [(act "act:covered" "act:grant-root")
              (act "act:none" "act:missing")
              (act "act:scope" "act:grant-root" :delete in-time "agent-a")
              (act "act:late" "act:grant-root" :deploy until "agent-a")]
        findings (overreach/scan acts [root])]
    (is (= ["act:none" "act:scope" "act:late"]
           (mapv :finding/act-id findings)))
    (is (= [:no-grant :out-of-scope :grant-expired]
           (mapv :finding/reason findings)))))

(deftest identity-does-not-substitute-for-scope
  (is (= [:out-of-scope]
         (reason-for [(act "act:self" "act:grant-root" :delete in-time "agent-a")]
                     [root]))))

(deftest interpretations-are-not-grants
  (let [reading (assoc (act "act:reading" "act:grant-root")
                       :act/stamp {:executor "agent-a" :signer "agent-a"
                                   :authority {:interpretation "e:1"}
                                   :executor-basis :session-bound})]
    (is (= [:interpretation-as-grant] (reason-for [reading] [root]))))
  (let [inferred (grant "act:inferred" "joe" "agent-a" nil
                        start until [:deploy] [] :inferred)]
    (is (= [:interpretation-as-grant]
           (reason-for [(act "act:inferred-use" "act:inferred")] [inferred])))))

(deftest half-open-time-and-grantee-are-classified
  (is (= [:grant-expired]
         (reason-for [(act "act:at-until" "act:grant-root" :deploy until "agent-a")]
                     [root])))
  (is (= [:grant-not-yet-valid]
         (reason-for [(act "act:early" "act:grant-root" :deploy
                           "2026-09-24T15:59:59Z" "agent-a")]
                     [root])))
  (is (= [:wrong-grantee]
         (reason-for [(act "act:other" "act:grant-root" :deploy in-time "agent-b")]
                     [root]))))

(deftest description-only-scope-never-passes
  (let [text-only (assoc-in root [:hx/props :grant/scope]
                            {:description "anything requested"})
        finding (first (overreach/scan [(act "act:text" "act:grant-root")]
                                      [text-only]))]
    (is (= :out-of-scope (:finding/reason finding)))))

(deftest broken-delegation-covers-parent-time-and-presence
  (let [child (grant "act:grant-child" "agent-a" "agent-b" "act:grant-root")
        child-act (act "act:child-use" "act:grant-child" :deploy in-time "agent-b")
        expired-parent (assoc-in root [:hx/props :grant/interval :until]
                                 "2026-09-24T16:15:00Z")]
    (testing "the child is locally valid, but its parent is expired"
      ;; The shared grant validator first detects that the child's interval
      ;; exceeds this shortened parent. That is a broken delegation, rather
      ;; than treating the invalid child as independently time-authorised.
      (is (= :broken-delegation
             (:finding/reason (first (overreach/scan [child-act]
                                                     [expired-parent child]))))))
    (testing "the claimed parent is absent"
      (is (= :broken-delegation
             (:finding/reason (first (overreach/scan [child-act] [child]))))))))

(deftest own-act-grant-checks-the-target-signer
  (let [wild (-> (grant "act:any-own" "joe" "*" nil)
                 (assoc-in [:hx/props :grant/scope :own-acts-only] true))
        other (assoc (act "act:other-own" "act:any-own")
                     :act/target-signer "codex-5")]
    (is (= [:not-own-act] (reason-for [other] [wild])))))

(deftest executor-verification-and-historical-coverage-are-distinct
  (let [declared (assoc-in (act "act:declared" "act:grant-root")
                           [:act/stamp :executor-basis] :declared)
        unstamped {:act/id "act:old" :act/kind :deploy :act/at in-time}
        report (overreach/scan-report [declared unstamped] [root])]
    (is (= [:unverified-executor :outside-coverage]
           (mapv :classification report)))
    (is (= [:unverified-executor]
           (mapv :finding/reason (overreach/scan [declared unstamped] [root]))))))

(deftest joe-operator-authority-needs-no-grant
  (let [operator {:act/id "act:joe" :act/kind :deploy :act/at in-time
                  :act/target-signer "joe"
                  :act/stamp {:executor "joe" :signer "joe"
                              :authority {:operator true}
                              :executor-basis :session-bound}}]
    (is (= :authorised
           (:classification (first (overreach/scan-report [operator] [])))))
    (is (= :unverified-executor
           (:classification
            (first (overreach/scan-report
                    [(assoc-in operator [:act/stamp :executor-basis] :declared)]
                    [])))))))

(deftest stamped-record-adapter
  (let [record {:id "act:card" :kind :pattern-card/selection
                :agent "agent-a" :session "s" :author "agent-a"
                :at in-time :pattern-id "p" :act/stamp (stamp "agent-a" "act:g")}
        adapted (overreach/record->act record)]
    (is (= "act:card" (:act/id adapted)))
    (is (= "agent-a" (:act/target-signer adapted)))
    (is (= (:act/stamp record) (:act/stamp adapted)))))

(deftest act-without-a-parseable-time-is-not-called-early
  (is (= [:act-time-unknown]
         (reason-for [(act "act:undated" "act:grant-root" :deploy nil "agent-a")]
                     [root])))
  (is (= [:act-time-unknown]
         (reason-for [(act "act:garbled" "act:grant-root" :deploy "yesterday" "agent-a")]
                     [root]))))

(deftest dispatch-edge-authority-is-confined-to-disclosures
  (let [dispatch-stamp {:executor "codex-5" :signer "codex-5"
                        :authority {:dispatch-edge "edge-evidence-1"}
                        :executor-basis :declared}
        disclosure {:act/id "act:disclosure" :act/kind :disclosure/choice
                    :act/at in-time :act/dispatch-edge "edge-evidence-1"
                    :act/stamp dispatch-stamp}
        withdrawal (assoc disclosure :act/id "act:withdrawal"
                          :act/kind :act/withdrawal
                          :act/target-kind :disclosure/choice)
        unrelated-withdrawal (assoc withdrawal :act/target-kind :offer/record)
        unrelated (assoc disclosure :act/id "act:deploy" :act/kind :deploy)]
    (is (= :authorised
           (:classification (overreach/classify-act disclosure []))))
    (is (= :authorised
           (:classification (overreach/classify-act withdrawal []))))
    (is (= :authority-kind-not-allowed
           (get-in (overreach/classify-act unrelated [])
                   [:finding :finding/reason])))
    (is (= :authority-kind-not-allowed
           (get-in (overreach/classify-act unrelated-withdrawal [])
                   [:finding :finding/reason])))))
