(ns futon3c.agency.rule-record-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [futon3c.agency.rule-record :as rule]))

(def fixture (edn/read-string (slurp "holes/labs/M-象-2000/P13a-requisition-rule.edn")))
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
