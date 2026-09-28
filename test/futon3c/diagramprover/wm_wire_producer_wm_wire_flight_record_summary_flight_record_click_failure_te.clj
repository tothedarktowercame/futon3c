(ns futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-record-click-failure-te
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.learning-trial-ledger :as learning-ledger]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire-r9-support :as r9-support]
            [futon3c.diagramprover.wm-wire-summary-products :as products]
            [futon3c.diagramprover.wm-wire :as w])
  (:import [java.security MessageDigest]))

(defn- runner-opts
  "The r9 wire's isolated options, with a judge that fails selection by
  throwing THROW-FN's exception."
  [throw-fn]
  (merge (hermetic/runner-repair-options)
         r9-support/hermetic-runner-defaults
         {:cohort? false :author "zai-5" :reviewer "codex-7" :repair-reviewer "codex-1"
          :phase-log-fn (fn [_])
          :roster-fn (fn [_] {:zai-5 {:status "idle" :invoke-ready? true}
                              :codex-7 {:status "idle" :invoke-ready? true}
                              :codex-1 {:status "idle" :invoke-ready? true}})
          :refresh-fn (fn [])
          :substrate-preflight-fn (fn [_] {:route :test})
          :code-state-fn (fn [] {:repo "/futon2" :git-sha "head" :git-dirty? false :repo-heads {}})
          :mode-flags-fn (fn [] {}) :version-stamp-fn identity :mission-fn (fn [t] {:id t})
          :repair-open-fn (constantly [])
          :repair-system-record-fn (fn [m] {:repair/id "repair-wire-1" :repair/class (:repair-class m)})
          :r16-park-fn (fn [_ _] {:ok true :id "park-wire" :status :parked})
          :delivery-qa-fn (fn [_ _] {:morning-brief/addendum-id "qa-wire"})
          :queue-fn identity
          :judge-fn (fn [_] (throw (throw-fn)))
          :construct-fn (fn [& _] (throw (ex-info "no construction expected" {})))}))

(defn- run-record
  "The run record run-opportunity! writes for a judge that throws
  THROW-FN's exception, in temp stores."
  [throw-fn]
  (with-redefs-fn {#'trace/default-trace-dir (w/tmp-dir "wire-trace")
                   #'runner/default-run-record-dir (w/tmp-dir "wire-run-records")
                   #'learning-ledger/default-root (w/tmp-dir "wire-learning")}
    #(binding [runner/*wm-status-reporting?* false]
       (edn/read-string (slurp (:run-record (runner/run-opportunity! (runner-opts throw-fn))))))))

(def eighth-run-record
  ;; a live run record written before WM-CLICK-REASON-I: no :failure key
  {:path (str w/spike-dir "/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
   :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"})

(defn- substrate-throw []
  (ex-info "substrate-2 mission registry unreachable" {}
           (java.net.ConnectException. "Connection refused")))

(defn- other-throw []
  (ex-info "the judge's model returned no parseable decision" {}))

(defn observe
  "The run record for THROW-FN read by record-summary, kept by
  record-click: {:writer the summary's :failure, :reader the click
  entry's :failure, :record-failure the run record's :failure}."
  [throw-fn]
  (let [record (run-record throw-fn)
        summary (fr/record-summary "M-t" "click-1" record)
        e (first (:clicks (flight/record-click
                           (flight/start {:target "M-t" :chosen-because {:kind :requested}}
                                         {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                         {:id "flight-wire"})
                           (merge summary {:wants [:t/b] :before {} :after {}}))))]
    {:writer (:failure summary)
     :reader (:failure e)
     :record-failure (:failure record)}))

(defn observe-absent
  "The pinned eighth run record read through the same vars: {:writer the
  summary's :failure, :reader the click entry's :failure}."
  []
  (let [record (w/read-record (:path eighth-run-record))
        summary (fr/record-summary "M-autoclock-in" "click-1" record)
        e (first (:clicks (flight/record-click
                           (flight/start {:target "M-autoclock-in" :chosen-because {:kind :requested}}
                                         {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                         {:id "flight-wire"})
                           (merge summary {:wants [:t/b] :before {} :after {}}))))]
    {:writer (:failure summary)
     :reader (:failure e)}))

(defn build-record []
  (let [p (observe substrate-throw) d (observe other-throw) a (observe-absent)
        [x y] (products/products :record-click :failure) rx (:record x) ry (:record y)]
    {:producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-record-click-failure-te-test
     :operation ['futon2.aif.full-loop-runner/run-opportunity! 'futon2.aif.flight-runner/record-summary 'futon2.aif.flight/record-click 'futon3c.diagramprover.wm-wire-summary-products/products]
     :inputs {:throw-fns [:substrate-unavailable :unparseable-decision] :live-record (:path eighth-run-record) :summary-product [:record-click :failure]}
     :fields {:positive p :different d :absent a
              :summary {:carriers-without-field-equal? (= (dissoc (:carrier x) :failure) (dissoc (:carrier y) :failure))
                        :a-copied? (= (get-in x [:carrier :failure]) (get-in rx [:clicks 0 :failure]))
                        :b-copied? (= (get-in y [:carrier :failure]) (get-in ry [:clicks 0 :failure]))
                        :values-differ? (not= (get-in rx [:clicks 0 :failure]) (get-in ry [:clicks 0 :failure]))
                        :records-without-field-equal? (= (update rx :clicks #(mapv (fn [c] (dissoc c :failure)) %)) (update ry :clicks #(mapv (fn [c] (dissoc c :failure)) %)))
                        :statuses [(:status rx) (:status ry)] :click-counts [(count (:clicks rx)) (count (:clicks ry))]}}
     :left-out {:temporary-stores "isolated trace, run-record, and learning stores under /tmp" :full-summary-records "the reader checks the recorded relations"}}))

(defn- txt [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- leaves [x] (letfn [(f [p v] (if (map? v) (mapcat (fn [[k x]] (f (conj p k) x)) v) [p]))] (f [] x)))
(defn- write! [x] (let [s (txt x) f (io/file "test/fixtures/wire-producers" (str "wm-wire-flight-record-summary-flight-record-click-failure-te@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "exists" {}))) (spit f s) (println f)))
(deftest producer-test (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [f (first (filter #(.startsWith (.getName %) "wm-wire-flight-record-summary-flight-record-click-failure-te@") (.listFiles (io/file "test/fixtures/wire-producers")))) e (edn/read-string (slurp f))] (doseq [p (leaves (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
