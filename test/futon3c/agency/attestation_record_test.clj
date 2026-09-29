(ns futon3c.agency.attestation-record-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.attestation-record :as sut]))

(def stamp {:executor "agent-a" :signer "agent-a"
            :authority {:grant "act:grant"} :executor-basis :declared})
(def harness {:kind :none :basis :producer-context :source-ref "test"})

(defn record [id pattern presentation proposal-author]
  (let [attester "agent-a"]
    (cond->
     {:id id :kind :pattern/attestation :schema 1 :pattern-id pattern
     :attester attester :at "2026-09-28T12:00:00Z"
     :use {:kind :pattern-card-selection :ref (str "act:selection-" id)}
     :presentation presentation
     :disposition (sut/disposition attester pattern presentation proposal-author)
     :act/stamp stamp :act/harness harness}
    proposal-author (assoc :proposal-author proposal-author))))

(deftest disposition-and-build-plan-count
  (let [echo (record "act:echo" "pattern/x"
                     {:ref "e:shown" :shown-pattern-ids ["pattern/x" "pattern/y"]} nil)
        independent (record "act:independent" "pattern/x"
                            {:ref "e:other" :shown-pattern-ids ["pattern/y"]} nil)
        result (sut/attestation-count [echo independent])]
    (is (= :shown-list-echo (:disposition echo)))
    (is (= :counts (:disposition independent)))
    (is (= {"pattern/x" 1} (:counts result)))
    (is (= [echo] (get-in result [:excluded :shown-list-echo])))))

(deftest exclusion-precedence-and-incoming-links
  (is (= :presentation-unknown
         (sut/disposition "xiang" "pattern/x"
                          {:shown-pattern-ids []} "xiang")))
  (is (= :proposer-warrant
         (sut/disposition "xiang" "pattern/x"
                          {:ref "e:shown" :shown-pattern-ids ["pattern/x"]} "xiang")))
  ;; Graph links are not an attestation input. No records means no count,
  ;; regardless of how many @how links the pattern has elsewhere.
  (is (= {:counts {} :excluded {}} (sut/attestation-count []))))

(deftest hyperedge-round-trip-has-no-self-endpoint
  (let [r (record "act:one" "象/限定随论"
                  {:ref "e:one" :shown-pattern-ids []} nil)
        edge (sut/->hyperedge r)]
    (is (= r (sut/hyperedge->record edge)))
    (is (= ["pattern:象/限定随论" "agent:agent-a" "act:selection-act:one"]
           (:hx/endpoints edge)))
    (is (not-any? #{"act:one"} (:hx/endpoints edge)))))

(deftest closed-record-and-derived-disposition
  (testing "unknown keys fail closed"
    (is (= :invalid-keys
           (try (sut/validate! (assoc (record "act:x" "p/x"
                                              {:ref "e:x" :shown-pattern-ids []} nil)
                                      :incoming-links 99))
                nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
  (testing "an unknown value is an absent key, never nil (futon1b drops nils)"
    (doseq [[label r] [["nil proposal author"
                        (assoc (record "act:x" "p/x" {:ref "e:x" :shown-pattern-ids []} nil)
                               :proposal-author nil)]
                       ["nil presentation ref"
                        (record "act:x" "p/x" {:ref nil :shown-pattern-ids []} nil)]]]
      (is (thrown? clojure.lang.ExceptionInfo (sut/validate! r)) label))
    (is (= "xiang" (:proposal-author
                    (sut/validate! (record "act:x" "p/x"
                                           {:ref "e:x" :shown-pattern-ids []} "xiang"))))))
  (testing "callers cannot assert a counting disposition"
    (is (= :disposition-mismatch
           (try (sut/validate! (assoc (record "act:x" "p/x"
                                              {:ref "e:x" :shown-pattern-ids ["p/x"]} nil)
                                      :disposition :counts))
                nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))
