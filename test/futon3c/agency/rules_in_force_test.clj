(ns futon3c.agency.rules-in-force-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.rule-timeline :as timeline]
            [futon3c.agency.rules-in-force :as rules]))

(def family "act:rule-family")
(def signer "codex-4")
(def grant-id "act:own-acts")
(def stamp {:executor signer :signer signer :authority {:grant grant-id}
            :executor-basis :session-bound})
(def grant
  {:hx/id grant-id :hx/type :grant/record
   :hx/props {:grant/grantor "joe" :grant/grantee "*" :grant/basis :explicit
              :grant/scope {:description "own withdrawals"
                            :act-kinds [:act/withdrawal]
                            :own-acts-only true}
              :grant/interval {:from "2026-09-24T00:00:00Z"}
              :grant/source {:id "e:joe" :author "joe"
                             :at "2026-09-24T00:00:00Z" :quote "own"}}})

(defn p13b-records []
  (let [requests (edn/read-string
                  (slurp "holes/labs/M-象-2000/P13b-requisition-versions.edn"))]
    (mapv (fn [index request]
            {:hx/id (if (zero? index) family (str "act:rule-version-" (inc index)))
             :hx/type :rule/record
             :hx/props (cond-> (assoc (:record request) :rule/family family)
                         (zero? index) (assoc :act/stamp stamp
                                             :rule/governs ["resident-a"]))})
          (range) requests)))

(defn withdrawal
  ([id author at] (withdrawal id author at family stamp))
  ([id author at target act-stamp]
   {:id id :kind :act/withdrawal :author author :at at :target target
    :status :effective :basis {:kind :self} :act/stamp act-stamp}))

(deftest p13b-timeline-semantics-are-unchanged
  (let [records (p13b-records)
        at "2026-09-25T21:00:00Z"
        expected (timeline/as-of records "kimi-requisition-20260924" at)
        result (rules/rules-in-force-as-of records [] [grant] at)]
    (is (= :followup-half-withdrawn (:effect expected)))
    (is (= expected (get-in result [:in-force 0 :answer])))
    (is (empty? (:ended result)))))

(deftest signer-withdrawal-ends-the-family-half-open
  (let [records (p13b-records)
        effect (withdrawal "act:end" signer "2026-09-26T12:00:00Z")
        before (rules/rules-in-force-as-of records [effect] [grant]
                                           "2026-09-26T11:59:59Z")
        at (rules/rules-in-force-as-of records [effect] [grant]
                                       "2026-09-26T12:00:00Z")]
    (is (= [family] (mapv :family (:in-force before))))
    (is (= [{:family family :by "act:end"}] (:ended at)))
    (is (empty? (:in-force at)))
    (is (= records (p13b-records)))))

(deftest governed-party-is-provisional-and-reversible
  (let [records (p13b-records)
        resident-stamp {:executor "resident-a" :signer "resident-a"
                        :authority {:grant "act:missing"}
                        :executor-basis :session-bound}
        proposal (withdrawal "act:proposal" "resident-a"
                             "2026-09-26T12:00:00Z" family resident-stamp)
        reversal (assoc (withdrawal "act:reverse" "resident-a"
                                    "2026-09-26T12:01:00Z" family resident-stamp)
                        :reverses "act:proposal")
        open (rules/rules-in-force-as-of records [proposal] [grant]
                                         "2026-09-26T12:00:30Z")
        reversed (rules/rules-in-force-as-of records [proposal reversal] [grant]
                                             "2026-09-26T12:02:00Z")]
    (is (= ["act:proposal"] (mapv :id (:provisional open))))
    (is (seq (:in-force open)))
    (is (empty? (:provisional reversed)))
    (is (seq (:in-force reversed)))))

(deftest unresolved-version-target-own-act-and-interpretation
  (let [records (p13b-records)
        unstamped (update-in records [0 :hx/props] dissoc :act/stamp)
        candidate (withdrawal "act:unresolved" signer "2026-09-26T12:00:00Z")
        unresolved (rules/rules-in-force-as-of unstamped [candidate] [grant]
                                               "2026-09-26T13:00:00Z")
        version-effect (withdrawal "act:version" signer "2026-09-26T12:00:00Z"
                                   "act:rule-version-2" stamp)
        unknown-effect (withdrawal "act:unknown" signer "2026-09-26T12:00:00Z"
                                   "act:not-a-rule" stamp)
        other-stamp {:executor "other" :signer "other" :authority {:grant grant-id}
                     :executor-basis :session-bound}
        other (withdrawal "act:other" "other" "2026-09-26T12:00:00Z"
                          family other-stamp)
        interpretation {:id "analysis:withdraw" :kind :interpretation
                        :author "xiang" :at "2026-09-26T12:00:00Z"
                        :target family}
        result (rules/rules-in-force-as-of records
                                           [version-effect unknown-effect other interpretation]
                                           [grant] "2026-09-26T13:00:00Z")]
    (is (= ["act:unresolved"] (mapv :id (:unresolved unresolved))))
    (is (seq (:in-force unresolved)))
    (is (= {:record-id "act:version" :reason :targets-version}
           (some #(when (= "act:version" (:record-id %)) %) (:ignored result))))
    (is (= {:record-id "act:unknown" :reason :unknown-target}
           (some #(when (= "act:unknown" (:record-id %)) %) (:ignored result))))
    (is (= {:record-id "act:other" :reason :not-own-act}
           (some #(when (= "act:other" (:record-id %)) %) (:ignored result))))
    (is (= {:record-id "analysis:withdraw" :reason :interpretation-not-effect}
           (some #(when (= "analysis:withdraw" (:record-id %)) %)
                 (:ignored result))))))
