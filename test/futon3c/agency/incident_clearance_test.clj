(ns futon3c.agency.incident-clearance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [futon3c.agency.incident-clearance :as clearance]
            [futon3c.agency.rule-record :as rule]
            [futon3c.agency.rule-timeline :as timeline]))

(def fixture (edn/read-string (slurp "holes/labs/M-象-2000/P14-clearance-record.edn")))
(def context (json/read-str (slurp "holes/labs/M-象-2000/P14-validation-context.json") :key-fn keyword))
(defn refusal [r]
  (try (clearance/validate! r context) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest real-record-and-mutations
  (is (= (:record fixture) (clearance/validate! (:record fixture) context)))
  (is (= 42 (count (get-in fixture [:record :clearance/compensation]))))
  (doseq [[label alter field]
          [["missing incident" #(dissoc % :clearance/incident) :clearance/incident]
           ["resolved without source" #(update % :clearance/resolved dissoc :source-id) :clearance/resolved]
           ["unknown resolution source" #(assoc-in % [:clearance/resolved :source-id] "missing") :clearance/resolved]
           ["missing measure ids" #(update % :clearance/measures-can-end dissoc :ids) :clearance/measures-can-end]
           ["empty measure ids" #(assoc-in % [:clearance/measures-can-end :ids] []) :clearance/measures-can-end]
           ["id does not resolve to a rule" #(assoc-in % [:clearance/measures-can-end :ids 0] "act:missing") :clearance/measures-can-end]
           ["empty list despite delivered notices" #(assoc % :clearance/compensation []) :clearance/compensation]
           ["missing notice" #(update % :clearance/compensation pop) :clearance/compensation]
           ["same count wrong membership" #(assoc-in % [:clearance/compensation 0 :evidence-id] "wrong-notice") :clearance/compensation]
           ["invented settlement" #(assoc-in % [:clearance/compensation 0 :status] :settled) :clearance/compensation]
           ["rule caused resolution" #(assoc-in % [:clearance/recognition :claim] "this rule caused resolution") :clearance/recognition]
           ["additional claim" #(assoc-in % [:clearance/recognition :right-measure?] true) :clearance/recognition]
           ["permission is not ending" #(assoc-in % [:clearance/measures-can-end :meaning] :withdrawn) :clearance/measures-can-end]
           ["invented grant" #(assoc-in % [:clearance/provenance :grant-status] :granted) :clearance/provenance]]]
    (testing label
      (let [bad (alter (:record fixture)) calls (atom [])]
        (is (= {:reason :invalid-incident-clearance :field field} (refusal bad)))
        (with-redefs [rule/request! (fn [& args] (swap! calls conj args))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (clearance/write! "http://unused" (assoc fixture :record bad) context))))
        (is (empty? @calls))))))

(deftest existing-id-of-wrong-type-or-owner-refused
  (doseq [ctx [(assoc-in context [:rules 0 :hx/type] "code/v05/commit")
               (assoc-in context [:rules 0 :hx/props :rule/incident :ref/id] "other-incident")]]
    (is (thrown? clojure.lang.ExceptionInfo (clearance/validate! (:record fixture) ctx)))))

(deftest permission-does-not-alter-application
  (let [records (mapv rule/payload (edn/read-string (slurp "holes/labs/M-象-2000/P13b-requisition-versions.edn")))
        with-clearance (conj records (clearance/payload fixture context))]
    (doseq [at ["2026-09-24T17:00:00Z" "2026-09-24T18:00:00Z" "2026-09-25T21:00:00Z"]]
      (is (= (timeline/as-of records "kimi-requisition-20260924" at)
             (timeline/as-of with-clearance "kimi-requisition-20260924" at))))
    (is (= (timeline/intervals records "kimi-requisition-20260924")
           (timeline/intervals with-clearance "kimi-requisition-20260924")))))

(deftest minted-readback-and-no-overwrite
  (let [p (clearance/payload fixture context) calls (atom [])]
    (is (= :incident/clearance (:hx/type p)))
    (is (true? (:hx/mint-id p)))
    (is (not (contains? p :hx/op)))
    (with-redefs [rule/request! (fn [_ method _ _]
                                 (swap! calls conj method)
                                 (if (= "POST" method) {:ok true :hx/id "act:test"}
                                     {:hyperedges [(assoc p :hx/id "act:test")]}))]
      (is (:verified? (clearance/write! "http://unused" fixture context)))
      (is (= ["POST" "GET"] @calls))))
  (is (thrown? clojure.lang.ExceptionInfo (clearance/payload (assoc fixture :hx/op "retract") context))))
