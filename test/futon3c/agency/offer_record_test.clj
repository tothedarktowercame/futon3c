(ns futon3c.agency.offer-record-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.offer-record :as offer]))

(def at "2026-09-28T12:00:00Z")
(def until "2026-09-28T13:00:00Z")
(def seat {:agent "claude-17" :session "session-1"})
(def stamp {:executor "claude-17" :signer "claude-17"
            :authority {:grant "act:grant"} :executor-basis :session-bound})
(def harness {:kind :none :basis :producer-context :source-ref "test:p11-1"})
(def record
  {:id "act:offer-a" :kind :offer/record :author "claude-17"
   :addressee "joe" :seat seat :at at :until until
   :options [{:option/id "1"
              :option/label "first packet"
              :option/scope {:description "Build the first packet"}}]
   :act/stamp stamp :act/harness harness})

(defn reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest typed-refusals
  (is (= :missing-author (reason #(offer/validate! (dissoc record :author)))))
  (is (= :missing-addressee (reason #(offer/validate! (dissoc record :addressee)))))
  (is (= :missing-seat (reason #(offer/validate! (dissoc record :seat)))))
  (is (= :missing-seat
         (reason #(offer/validate! (assoc record :seat {:agent "claude-17"})))))
  (is (= :no-options (reason #(offer/validate! (assoc record :options [])))))
  (is (= :duplicate-option-ids
         (reason #(offer/validate!
                   (update record :options conj (first (:options record)))))))
  (is (= :missing-option-scope
         (reason #(offer/validate!
                   (assoc-in record [:options 0] {:option/id "1"})))))
  (is (= :invalid-interval
         (reason #(offer/validate! (assoc record :until at)))))
  (is (= :invalid-grant-until
         (reason #(offer/validate!
                   (assoc-in record [:options 0 :option/scope :grant-until]
                             "not-an-instant")))))
  (is (= :invalid-grant-until
         (reason #(offer/validate!
                   (assoc-in record [:options 0 :option/scope :grant-until] at)))))
  (is (= :invalid-act-kinds
         (reason #(offer/validate!
                   (assoc-in record [:options 0 :option/scope :act-kinds]
                             ["not-a-keyword"])))))
  (is (= :missing-act-stamp
         (reason #(offer/validate! (dissoc record :act/stamp)))))
  (is (= :invalid-grant-id
         (reason #(offer/validate!
                   (assoc-in record [:act/stamp :authority] {:grant "grant"})))))
  (is (= :stamp-signer-mismatch
         (reason #(offer/validate!
                   (assoc-in record [:act/stamp :signer] "codex-5"))))))

(deftest description-only-scope-is-a-valid-proposal
  (is (= record (offer/validate! record))))

(deftest display-lines-show-only-structured-finite-grants
  (let [long-label (str "line one\n" (apply str (repeat 300 "x")))
        shown (assoc record :options
                     [{:option/id "1" :option/label "grant choice"
                       :option/scope {:description "grant"
                                      :act-kinds [:x :y]
                                      :rule-ids ["act:rule"]
                                      :grant-until "2026-09-28T12:30:00Z"}}
                      {:option/id "2" :option/label "agreement"
                       :option/scope {:description "only"}}
                      {:option/id "3" :option/label "description deadline"
                       :option/scope {:description "not authority"
                                      :grant-until "2026-09-28T12:30:00Z"}}
                      {:option/id "4" :option/label long-label
                       :option/scope {:description "label test"}}])
        lines (offer/display-lines shown)]
    (is (= "offer act:offer-a from claude-17 (reply yes <n>, or yes act:offer-a <n>):"
           (first lines)))
    (is (= "  1  grant choice  — grants: act-kinds [:x :y] rule-ids [\"act:rule\"] until 2026-09-28T12:30:00Z"
           (nth lines 1)))
    (is (str/includes? (nth lines 2) "agreement only, no grant"))
    (is (str/includes? (nth lines 3) "agreement only, no grant"))
    (is (not (str/includes? (nth lines 4) "\n")))
    (let [[_ label] (re-find #"^  4  (.*)  — agreement" (nth lines 4))]
      (is (= 120 (count label))))))

(deftest hyperedge-round-trip-keeps-time-harness-and-schema
  (let [edge (offer/record->hyperedge record)]
    (is (= 1 (get-in edge [:hx/props :offer/schema])))
    (is (= at (get-in edge [:hx/props :at])))
    (is (= harness (get-in edge [:hx/props :act/harness])))
    (is (= record (offer/hyperedge->record edge)))
    (is (= record (offer/hyperedge->record (dissoc edge :hx/valid-time))))))

(defn active [records t]
  (offer/active-offers-as-of records seat t))

(deftest expiry-is-half-open
  (is (= [record] (:offers (active [record] "2026-09-28T12:59:59Z"))))
  (is (empty? (:offers (active [record] until))))
  (is (empty? (:offers (active [record] "2026-09-28T13:00:01Z")))))

(deftest withdrawal-removes-offer-without-deleting-it
  (let [withdrawal {:id "act:withdraw-offer" :kind :act/withdrawal
                    :author "claude-17" :at "2026-09-28T12:30:00Z"
                    :target (:id record) :status :effective
                    :basis {:kind :self}}]
    (is (= [record] (:offers (active [record withdrawal] "2026-09-28T12:29:59Z"))))
    (is (empty? (:offers (active [record withdrawal] "2026-09-28T12:30:00Z"))))
    (is (= record (first [record withdrawal])))))

(deftest provisional-withdrawal-is-reversible
  (let [withdrawal {:id "act:provisional" :kind :act/withdrawal :author "xiang"
                    :at "2026-09-28T12:20:00Z" :target (:id record)
                    :status :provisional :basis {:kind :provisional-interpretation}}
        reversal {:id "act:reversal" :kind :act/withdrawal :author "joe"
                  :at "2026-09-28T12:25:00Z" :target (:id record)
                  :status :effective :basis {:kind :self}
                  :reverses (:id withdrawal)}]
    (is (empty? (:offers (active [record withdrawal] "2026-09-28T12:24:00Z"))))
    (is (= [record]
           (:offers (active [record withdrawal reversal] "2026-09-28T12:25:00Z"))))))

(deftest acceptance-removes-offer-from-its-time
  (let [agreement {:id "act:agreement" :kind :agreement/record
                   :agreement/offer (:id record)
                   :agreement/acceptance-evidence "emacs:joe-turn"
                   :agreement/option-id "1"
                   :agreement/scope {:description "Build the first packet"}
                   :agreement/offeror "claude-17"
                   :agreement/acceptor "joe"
                   :agreement/at "2026-09-28T12:40:00Z"
                   :act/stamp {:executor "joe" :signer "joe"
                               :authority {:operator true}
                               :executor-basis :session-bound}
                   :act/harness harness}]
    (is (= [record] (:offers (active [record agreement] "2026-09-28T12:39:59Z"))))
    (is (empty? (:offers (active [record agreement] "2026-09-28T12:40:00Z"))))))

(deftest seats-are-exact-and-may-have-multiple-offers
  (let [second (assoc record :id "act:offer-b" :at "2026-09-28T12:05:00Z")
        other (assoc record :id "act:offer-other"
                     :seat {:agent "claude-17" :session "session-2"})
        result (active [record second other] "2026-09-28T12:10:00Z")]
    (is (= [record second] (:offers result)))
    (is (empty? (:ignored result)))))
