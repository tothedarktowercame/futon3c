(ns futon3c.diagramprover.finding-view-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.records :as records]
            [futon3c.diagramprover.arxana-records :as ar]
            [futon3c.diagramprover.arxana-records-test :as real]
            [futon3c.diagramprover.finding-view :as view]))

(def t0 "2026-11-17T00:00:00.000Z")
(def t1 "2026-11-17T01:00:00.000Z")
(def t2 "2026-11-17T02:00:00.000Z")
(def t3 "2026-11-17T03:00:00.000Z")
(defn rule [n]
  (ar/rule-record {:name n :pattern [:fixture] :guards [] :rewrite-doc [:replacement]} :fixture))
(defn finding [a b]
  (:finding
   (ar/finding-records
    {:finding/rules [(:id a) (:id b)] :finding/overlap [:fixture]
     :finding/status :non-joining :finding/kinds #{:root} :finding/evaluation :failing
     :finding/occurrences 1 :finding/positions [[]] :finding/witness-path []
     :finding/observations {:comparison :not-requested :sides [#{} #{}]}
     :finding/pins {:checker-sha "fixture" :schema-digest "fixture" :mode :unordered
                    :max-states 10 :event-time t0 :host/locator :transcribed}}
    {(:id a) (:id a) (:id b) (:id b)} {})))
(def r1 (rule :r1))
(def r2 (rule :r2))
(def r3 (rule :r3))
(def r1-prime (rule :r1-prime))
(def f12 (finding r1 r2))
(def f23 (finding r2 r3))
(def fprime (finding r1-prime r2))
(def supersession (view/supersedes-record (:id r1-prime) (:id r1) t1))
(def base [r1 r2 r3 r1-prime f12 f23 fprime supersession])

(deftest direct-predecessor-planted-check
  (is (= #{} (view/stale-findings base t0)))
  (is (= #{(:id f12)} (view/stale-findings base t1)))
  (is (false? (:stale? (view/finding-view base (:id f23) t1))))
  (is (false? (:stale? (view/finding-view base (:id fprime) t1))))
  (is (= [(:id supersession)] (:stale-because (view/finding-view base (:id f12) t1))))
  ;; Mutation: ask whether a finding names the successor, not predecessor.
  (let [wrong (set (for [f [f12 f23 fprime]
                         :when (some #(= (:id r1-prime) (get-in % [:target :clusion/id])) (:endpoints f))]
                     (:id f)))]
    (is (= #{(:id fprime)} wrong))
    (is (not= wrong (view/stale-findings base t1))))
  (is (= (:id supersession)
         (:id (view/supersedes-record (:id r1-prime) (:id r1) t2))))
  (is (= (:id supersession) (records/clusion-id supersession))))

(deftest residual-validity-and-met-mutation
  (let [residual (assoc (ar/residual-record (:id f12) {:scope "fixture" :release "pilot" :reason "known defect"})
                        :event-time t2)
        rs (conj base residual)
        before (view/finding-view rs (:id f12) t1)
        after (view/finding-view rs (:id f12) t2)]
    (is (nil? (:residual before)))
    (is (= {:stale :failing} (:display before)))
    (is (= residual (:residual after)))
    (is (= :failing (:evaluation after)))
    (is (= {:stale :failing-accepted} (:display after)))
    (is (not= (:display after) {:stale :met}))
    (is (= :failing-accepted (:display (view/finding-view (remove #(= supersession %) rs) (:id f12) t2))))
    (let [later (assoc (ar/residual-record (:id f12) {:scope "fixture" :release "later" :reason "second decision"}) :event-time t3)]
      (is (= residual (:residual (view/finding-view (conj rs later) (:id f12) t2))))
      (is (= later (:residual (view/finding-view (reverse (conj rs later)) (:id f12) t3)))))))

(deftest real-a02j03-finding-becomes-stale
  (let [{:keys [rules ids converted]} @real/sample
        f (:finding converted)
        variant (ar/rule-record (assoc (first real/schemas) :rewrite-doc [:fixture-variant]) :apm-printer)
        s (view/supersedes-record (:id variant) (:beta ids) t1)
        rs (into rules [f variant s])]
    (is (= #{} (view/stale-findings rs t0)))
    (is (= #{(:id f)} (view/stale-findings rs t1)))
    (is (= [(:id s)] (:stale-because (view/finding-view rs (:id f) t1))))
    (is (not= (view/stale-findings (remove #(= s %) rs) t1) (view/stale-findings rs t1)))))

(deftest inclusive-time-and-stale-display-for-other-evaluations
  (doseq [ev [:met :settled :unknown]]
    (let [f (records/make-clusion (assoc-in f12 [:asserted-content :evaluation] ev))
          rs [f supersession]]
      (is (= ev (:display (view/finding-view rs (:id f) t0))))
      (is (= {:stale ev} (:display (view/finding-view rs (:id f) "2026-11-17T02:00:00+01:00"))))))
  (is (thrown? clojure.lang.ExceptionInfo (view/finding-view base (:id f12) "not-an-instant")))
  (is (thrown? clojure.lang.ExceptionInfo
               (view/finding-view [f12 (dissoc supersession :event-time)] (:id f12) t1))))
