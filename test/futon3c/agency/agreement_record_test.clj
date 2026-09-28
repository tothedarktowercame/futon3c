(ns futon3c.agency.agreement-record-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.agreement-record :as agreement]
            [futon3c.agency.offer-record :as offer-record]))

(def harness {:kind :none :basis :producer-context :source-ref "test:p11-3"})
(def offer-stamp {:executor "agent-a" :signer "agent-a"
                  :authority {:grant "act:offer-grant"}
                  :executor-basis :declared})
(def seat {:agent "agent-a" :session "session-a"})
(def offer
  {:id "act:offer-a" :kind :offer/record :author "agent-a" :addressee "joe"
   :seat seat :at "2026-09-28T14:00:00Z" :until "2026-09-28T15:00:00Z"
   :options [{:option/id "1" :option/label "one"
              :option/scope {:description "one"}}
             {:option/id "2" :option/label "two"
              :option/scope {:description "two"}}]
   :act/stamp offer-stamp :act/harness harness})
(def agreement-record
  {:id "act:agreement-a" :kind :agreement/record
   :agreement/offer "act:offer-a"
   :agreement/acceptance-evidence "emacs:joe-turn"
   :agreement/option-id "2"
   :agreement/scope {:description "two"}
   :agreement/offeror "agent-a" :agreement/acceptor "joe"
   :agreement/at "2026-09-28T14:30:00Z"
   :act/stamp agreement/operator-stamp :act/harness harness})

(defn reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest acceptance-grammar-is-classical
  (doseq [[input expected]
          [["yes" {:offer-id nil :option-id nil}]
           [" YES. " {:offer-id nil :option-id nil}]
           ["yes 2" {:offer-id nil :option-id "2"}]
           ["yes 2!" {:offer-id nil :option-id "2"}]
           ["yes act:offer-a" {:offer-id "act:offer-a" :option-id nil}]
           ["Yes act:offer-a 2." {:offer-id "act:offer-a" :option-id "2"}]
           ["yes please" nil]
           ["yes, but" nil]
           ["yes 2 3" nil]
           ["not yes" nil]
           ["yes?" nil]]]
    (is (= expected (agreement/parse-acceptance input)) input)))

(deftest resolver-does-not-break-ties-by-recency
  (testing "one offer and one option"
    (let [single (assoc offer :options [(first (:options offer))])]
      (is (= {:accept {:offer single :option (first (:options single))}}
             (agreement/resolve-acceptance {:offers [single]}
                                           (agreement/parse-acceptance "yes"))))))
  (testing "bare yes leaves two options ambiguous"
    (is (= #{{:offer-id "act:offer-a" :option-id "1"}
             {:offer-id "act:offer-a" :option-id "2"}}
           (set (get-in (agreement/resolve-acceptance
                         {:offers [offer]} (agreement/parse-acceptance "yes"))
                        [:ambiguous :candidates])))))
  (testing "option id shared by two offers is ambiguous"
    (let [other (assoc offer :id "act:offer-b" :at "2026-09-28T14:01:00Z")]
      (is (= #{"act:offer-a" "act:offer-b"}
             (set (map :offer-id
                       (get-in (agreement/resolve-acceptance
                                {:offers [offer other]}
                                (agreement/parse-acceptance "yes 2"))
                               [:ambiguous :candidates]))))))))

(deftest resolver-refuses-invisible-offer-and-unknown-option
  (let [withdrawal {:id "act:withdraw" :kind :act/withdrawal :author "agent-a"
                    :target (:id offer) :status :effective :basis {:kind :self}
                    :at "2026-09-28T14:10:00Z"}
        visible (offer-record/active-offers-as-of
                 [offer withdrawal] seat "2026-09-28T14:20:00Z")]
    (is (= {:refused {:reason :unknown-offer}}
           (agreement/resolve-acceptance
            visible (agreement/parse-acceptance "yes act:offer-a")))))
  (is (= {:refused {:reason :unknown-option}}
         (agreement/resolve-acceptance
          {:offers [offer]} (agreement/parse-acceptance "yes act:offer-a 3")))))

(deftest agreement-is-checked-against-exact-offer-choice
  (is (= agreement-record
         (agreement/validate-against-offer! agreement-record offer)))
  (is (= :scope-mismatch
         (reason #(agreement/validate-against-offer!
                   (assoc-in agreement-record [:agreement/scope :extra] true)
                   offer))))
  (is (= :invalid-operator-stamp
         (reason #(agreement/validate!
                   (assoc agreement-record :act/stamp
                          {:executor "joe" :signer "joe"
                           :authority {:grant "act:grant"}
                           :executor-basis :session-bound})))))
  (is (= :offer-not-visible
         (reason #(agreement/validate-against-offer!
                   (assoc agreement-record :agreement/at (:until offer)) offer))))
  (is (= :offer-not-visible
         (reason #(agreement/validate-against-offer!
                   (assoc agreement-record :agreement/at "2026-09-28T15:00:01Z")
                   offer)))))

(deftest agreement-shape-is-closed
  (is (= :unexpected-key
         (reason #(agreement/validate! (assoc agreement-record :extra true)))))
  (is (= :missing-field
         (reason #(agreement/validate! (dissoc agreement-record
                                               :agreement/acceptance-evidence)))))
  (is (= :acceptor-not-operator
         (reason #(agreement/validate! (assoc agreement-record
                                              :agreement/acceptor "agent-a"))))))

(deftest agreement-hyperedge-round-trips-losslessly
  (let [edge (agreement/record->hyperedge agreement-record)]
    (is (= :agreement/record (:hx/type edge)))
    (is (= ["act:offer-a" "agent:agent-a" "agent:joe" "emacs:joe-turn"]
           (:hx/endpoints edge)))
    (is (= 1 (get-in edge [:hx/props :agreement/schema])))
    (is (= agreement-record (agreement/hyperedge->record edge)))
    (is (= agreement-record
           (agreement/hyperedge->record (dissoc edge :hx/valid-time))))))
