(ns futon3c.arxana.formal-c1-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.type-registry :as types]))

(defn interpretation [intent target]
  {:type-revision [:interpretation 1]
   :endpoints [{:role :source :kind :manifestation-scope
                :target {:artifact/revision-id "rev:1" :locator {:fragment "s1"}}}
               {:role :intent :kind :entity :target {:intent/id intent}}
               {:role :target :kind :external-ref :target target}]
   :asserted-content {:reading/state :candidate}})

(deftest role-swap-changes-id--mirrors-lean-Identity-role_swap_changes_id
  (let [proposal {:type-revision [:pattern/proposal 1]
                  :endpoints [{:role :turn :kind :external-ref :target {:ref/id "turn-1"}}
                              {:role :pattern :kind :external-ref :target {:ref/id "pattern-1"}}]
                  :asserted-content {:intent :proposed}}
        swapped (assoc proposal :endpoints
                       [{:role :turn :kind :external-ref :target {:ref/id "pattern-1"}}
                        {:role :pattern :kind :external-ref :target {:ref/id "turn-1"}}])]
    (is (not= (records/clusion-id proposal) (records/clusion-id swapped)))))

(deftest type-revision-changes-id--mirrors-lean-Identity-type_revision_in_id
  (let [v1 (types/declaration [:interpretation 1])
        v2 (assoc v1 :type/revision 2)
        registry (:registry (types/register-declaration types/registry-v1
                                                         (dissoc v2 :type/digest)))
        c1 (interpretation :delegate {:ref/id "claude-19"})
        c2 (assoc c1 :type-revision [:interpretation 2])]
    (is (not= (records/clusion-id registry c1)
              (records/clusion-id registry c2)))))

(deftest two-interpreters-one-claim--mirrors-lean-Store-two_interpreters_one_claim
  (let [claim (interpretation :constrain {:ref/id "M-formal-arxana"})
        claim-id (records/clusion-id claim)
        base {:subject-clusion-id claim-id
              :basis :model :standing :unverified :coverage :complete
              :method {:id "xiang/read" :version 1}
              :content {:reading :constrain} :source-refs []}
        a1 (records/make-attestation (assoc base :producer "象-sonnet"))
        a2 (records/make-attestation (assoc base :producer "象-haiku"))]
    (is (= claim-id (records/clusion-id claim)))
    (is (not= (:id a1) (:id a2)))
    (is (= claim-id (:subject-clusion-id a1) (:subject-clusion-id a2)))
    (is (nil? (:producer claim)))
    (is (nil? (:method claim)))))

(deftest declaration-revision-is-immutable
  (let [declaration (types/declaration [:interpretation 1])]
    (is (false? (:created? (types/register-declaration types/registry-v1
                                                        (dissoc declaration :type/digest)))))
    (is (= :arxana.type/in-place-edit-refused
           (try
             (types/register-declaration types/registry-v1
                                         (-> declaration
                                             (dissoc :type/digest)
                                             (assoc :identity/content-fields [:different])))
             nil
             (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))

(deftest c1-declarations-are-resolved
  ;; C1's declarations plus the later formal-Arxana relation types.
  (is (= 55 (count types/registry-v1)))
  (is (every? #(contains? types/registry-v1 %)
              [[:code-definition 1] [:doc-entry 1]
               [:kernel-builds 1] [:declaration-location 1]
               [:correspondence 1] [:generation-run 1]
               [:synthetic-mutation 1] [:formalization-coverage 1]
               [:geometry/entity 1] [:geometry/map 1]
               [:geometry/symmetry 1] [:geometry/projection 1]
               [:geometry/preserves 1] [:geometry/satisfies 1]
               [:geometry/fibre-of 1] [:geometry/member 1]
               [:geometry/value 1] [:geometry/placement 1]
               [:geometry/fibre-of 2] [:geometry/named 1]
               [:geometry/selection 1]]))
  (doseq [[_ declaration] types/registry-v1]
    (is (string? (:type/digest declaration)))
    (is (types/convergence-policies (:convergence/policy declaration)))
    (is (types/cycle-policies (:recursion/cycle-policy declaration)))))

(deftest golden-record-ids-are-pinned
  (let [resource (io/file "test/resources/arxana/formal_arxana/c1/golden-records.edn")
        golden (edn/read-string (slurp resource))]
    (is (= #{:f1 :repl-circus} (set (keys golden))))
    (doseq [[_ records] golden
            {:keys [record type-revision content expected-id attestations]} records]
      (is (= expected-id
             (if record
               (records/clusion-id record)
               (records/record-id type-revision content))))
      (doseq [{attestation :record expected-attestation-id :expected-id} attestations]
        (is (= expected-attestation-id (records/attestation-id attestation)))))))

(deftest intent-vocabulary-generated-presentations-do-not-drift
  (let [{:keys [exit out err]}
        (shell/sh "bb" "scripts/generate_intent_vocabulary.clj" "--check"
                  :env (assoc (into {} (System/getenv))
                              "FUTON3C_ROOT" (System/getProperty "user.dir")
                              "FUTON3_ROOT" "/home/joe/code/futon3"
                              "REPLY_PROFORMA" "/home/joe/code/reply-proforma.md"))]
    (is (zero? exit) (str out err))
    (is (.contains out "presentations agree"))))
