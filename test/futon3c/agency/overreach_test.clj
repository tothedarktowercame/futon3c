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
                               :act-kinds kinds
                               :rule-ids rules}
                 :grant/interval (cond-> {:from from} to (assoc :until to))
                 :grant/source {:id "e-source" :author grantor :at from :quote "do it"}
                 :grant/basis basis}
                parent (assoc :grant/parent parent))}))

(def root (grant "act:grant-root" "joe" "agent-a" nil))

(defn act
  ([id authority] (act id authority :deploy in-time "agent-a"))
  ([id authority kind at executor]
   {:act/id id :act/kind kind :act/rule-id nil :act/executor executor
    :act/at at :act/authority authority :act/signer executor}))

(defn reason-for [acts grants]
  (mapv :finding/reason (overreach/scan acts grants)))

(deftest acceptance-four-cases-report-only-uncovered-acts
  (let [acts [(act "act:covered" "act:grant-root")
              (act "act:none" nil)
              (act "act:scope" "act:grant-root" :delete in-time "agent-a")
              (act "act:late" "act:grant-root" :deploy until "agent-a")]
        findings (overreach/scan acts [root])]
    (is (= ["act:none" "act:scope" "act:late"]
           (mapv :finding/act-id findings)))
    (is (= [:no-grant :out-of-scope :grant-expired]
           (mapv :finding/reason findings)))))

(deftest identity-does-not-substitute-for-scope
  (let [a (assoc (act "act:self" "act:grant-root" :delete in-time "agent-a")
                 :act/signer "agent-a")]
    (is (= [:out-of-scope] (reason-for [a] [root])))))

(deftest interpretations-are-not-grants
  (is (= [:interpretation-as-grant]
         (reason-for [(act "act:reading" {:kind :interpretation :ref "e:1"})] [root])))
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
    (is (= :out-of-scope (:finding/reason finding)))
    (is (= :description-only-scope
           (get-in finding [:finding/detail :explanation])))))

(deftest broken-delegation-covers-parent-time-and-presence
  (let [child (grant "act:grant-child" "agent-a" "agent-b" "act:grant-root")
        child-act (act "act:child-use" "act:grant-child" :deploy in-time "agent-b")
        expired-parent (assoc-in root [:hx/props :grant/interval :until]
                                 "2026-09-24T16:15:00Z")]
    (testing "the child is locally valid, but its parent is expired"
      (let [finding (first (overreach/scan [child-act] [expired-parent child]))]
        (is (= :broken-delegation (:finding/reason finding)))
        (is (= :grant-expired
               (get-in finding [:finding/detail :explanation])))))
    (testing "the claimed parent is absent"
      (let [finding (first (overreach/scan [child-act] [child]))]
        (is (= :broken-delegation (:finding/reason finding)))
        (is (= :missing-parent
               (get-in finding [:finding/detail :explanation])))))))

(deftest act-without-a-parseable-time-is-not-called-early
  (is (= [:act-time-unknown]
         (reason-for [(act "act:undated" "act:grant-root" :deploy nil "agent-a")]
                     [root])))
  (is (= [:act-time-unknown]
         (reason-for [(act "act:garbled" "act:grant-root" :deploy "yesterday" "agent-a")]
                     [root]))))
