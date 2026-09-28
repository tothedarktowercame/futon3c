(ns futon3c.agency.rule-record-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [futon3c.agency.rule-record :as rule]
            [futon3c.agency.rules-in-force :as rules]))

(def fixture (edn/read-string (slurp "holes/labs/M-象-2000/P13a-requisition-rule.edn")))
(def p13b (edn/read-string (slurp "holes/labs/M-象-2000/P13b-requisition-versions.edn")))
(def family-id "act:schema-2-family")
(def schema-2-stamp
  {:executor "codex-4" :signer "codex-4"
   :authority {:grant "act:rule-author-grant"}
   :executor-basis :session-bound})
(def schema-2-record
  (assoc (:record (first p13b))
         :rule/schema 2 :rule/family :self :rule/governs []
         :act/stamp schema-2-stamp))
(def schema-2-request
  (assoc (first p13b) :record schema-2-record))
(defn refusal [record]
  (try (rule/validate! record) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest required-refusals-before-any-write
  (doseq [[label alter field]
          [["empty HOWEVER" #(assoc % :rule/however {}) [:rule/however :failure-mode]]
           ["whitespace HOWEVER" #(assoc-in % [:rule/however :failure-mode] "  ") [:rule/however :failure-mode]]
           ["missing accomplishment" #(dissoc % :rule/accomplishment) :rule/accomplishment]
           ["empty accomplishment" #(assoc-in % [:rule/accomplishment :description] "") [:rule/accomplishment :description]]
           ["missing incident" #(dissoc % :rule/incident) :rule/incident]
           ["blank incident" #(assoc-in % [:rule/incident :ref/id] " ") :rule/incident]]]
    (testing label
      (let [bad (alter (:record fixture)) calls (atom [])]
        (is (= {:reason :invalid-rule-record :field field} (refusal bad)))
        (with-redefs [rule/request! (fn [& args] (swap! calls conj args))]
          (is (thrown? clojure.lang.ExceptionInfo (rule/write! "http://unused" (assoc fixture :record bad)))))
        (is (empty? @calls) "Refusal precedes the write port")))))

(deftest unknown-however-needs-review-date
  (let [r (assoc (:record fixture) :rule/however
                 {:status :unknown :failure-mode "failure mode unknown" :review-at "2026-10-04"})]
    (is (= r (rule/validate! r)))
    (is (= :invalid-rule-record (:reason (refusal (update r :rule/however dissoc :review-at)))))
    (is (= :invalid-rule-record (:reason (refusal (assoc-in r [:rule/however :review-at] "sometime")))))))

(deftest minted-only-and-explicit-valid-time
  (let [p (rule/payload fixture)]
    (is (= :rule/record (:hx/type p)))
    (is (true? (:hx/mint-id p)))
    (is (= (:valid-from fixture) (:hx/valid-time p)))
    (is (= (:idempotency-key fixture) (:hx/idempotency-key p)))
    (is (not (contains? p :hx/id)))
    (is (not (contains? p :hx/op))))
  (doseq [r [(assoc fixture :hx/id "existing") (assoc fixture :hx/op "retract")
             (assoc fixture :valid-from "yesterday")]]
    (is (thrown? clojure.lang.ExceptionInfo (rule/payload r)))))

(deftest known-however-signal-and-world-assumption-are-required
  (doseq [r [(update (:record fixture) :rule/however dissoc :signal)
             (dissoc (:record fixture) :rule/world-assumption)]]
    (is (= :invalid-rule-record (:reason (refusal r))))))

(deftest typed-rule-readback-and-idempotency-receipt
  (let [calls (atom []) p (rule/payload fixture)
        stored (assoc (select-keys p [:hx/type :hx/endpoints :hx/props]) :hx/id "act:test")]
    (with-redefs [rule/request!
                  (fn [_ method path value]
                    (swap! calls conj [method path value])
                    (if (= method "POST") {:ok true :hx/id "act:test" :no-op? true}
                        {:hyperedges [stored]}))]
      (is (= {:ok true :hx/id "act:test" :no-op? true :verified? true}
             (rule/write! "http://unused" fixture)))
      (is (= ["POST" "GET"] (mapv first @calls)))
      (is (re-find #"valid-as-of=" (second (second @calls))))))
  (with-redefs [rule/request! (fn [_ method _ _]
                               (if (= method "POST") {:ok true :hx/id "act:test"}
                                   {:hyperedges []}))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                         (rule/write! "http://unused" fixture)))))

(deftest schema-2-requires-family-stamp-and-governs
  (doseq [[record expected]
          [[(dissoc schema-2-record :rule/family) :missing-rule-family]
           [(dissoc schema-2-record :act/stamp) :missing-act-stamp]
           [(dissoc schema-2-record :rule/governs) :missing-rule-governs]
           [(assoc schema-2-record :rule/governs ["agent-a" ""]) :invalid-rule-governs]
           [(assoc-in schema-2-record [:act/stamp :authority]
                      {:interpretation "analysis:1"}) :interpretation-not-authority]
           [(assoc-in schema-2-record [:act/stamp :signer] "other")
            :stamp-author-mismatch]]]
    (is (= expected (:reason (refusal record))) (str expected))))

(deftest self-family-and-version-round-trip-into-one-projection
  (let [first-payload (rule/payload schema-2-request)
        first-edge (-> first-payload
                       (dissoc :hx/mint-id :hx/idempotency-key)
                       (assoc :hx/id family-id))
        version-record (assoc (:record (second p13b))
                              :rule/schema 2 :rule/family family-id :rule/governs []
                              :act/stamp schema-2-stamp)
        version-request (assoc (second p13b) :record version-record)
        version-payload (rule/payload version-request)
        version-edge (-> version-payload
                         (dissoc :hx/mint-id :hx/idempotency-key)
                         (assoc :hx/id "act:schema-2-version"))
        at "2026-09-25T21:00:00Z"
        projected (rules/rules-in-force-as-of [first-edge version-edge] [] [] at)]
    (is (= :self (get-in first-edge [:hx/props :rule/family])))
    (is (= family-id (get-in version-edge [:hx/props :rule/family])))
    (is (= schema-2-stamp (get-in first-edge [:hx/props :act/stamp])))
    (is (= [family-id] (mapv :family (:in-force projected))))
    (is (= :followup-half-withdrawn
           (get-in projected [:in-force 0 :answer :effect])))))

(deftest later-version-write-checks-the-live-family-first-record
  (let [first-payload (rule/payload schema-2-request)
        first-edge (-> first-payload
                       (dissoc :hx/mint-id :hx/idempotency-key)
                       (assoc :hx/id family-id))
        version-request (assoc-in (second p13b) [:record :rule/schema] 2)
        version-request (-> version-request
                            (assoc-in [:record :rule/family] family-id)
                            (assoc-in [:record :rule/governs] [])
                            (assoc-in [:record :act/stamp] schema-2-stamp))
        payload (rule/payload version-request)
        stored (-> payload
                   (dissoc :hx/mint-id :hx/idempotency-key :hx/valid-time)
                   (assoc :hx/id "act:version"))
        calls (atom [])]
    (with-redefs [rule/request!
                  (fn [_ method path value]
                    (swap! calls conj [method path value])
                    (cond
                      (= path "/api/alpha/hyperedge/act%3Aschema-2-family") first-edge
                      (= method "POST") {:ok true :hx/id "act:version"}
                      :else {:hyperedges [stored]}))]
      (is (true? (:verified? (rule/write! "http://store" version-request))))
      (is (= ["GET" "POST" "GET"] (mapv first @calls))))
    (with-redefs [rule/request! (fn [& _]
                                 (throw (ex-info "missing" {:status 404})))]
      (is (= :family-not-found
             (:reason (try (rule/write! "http://store" version-request)
                           (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))
