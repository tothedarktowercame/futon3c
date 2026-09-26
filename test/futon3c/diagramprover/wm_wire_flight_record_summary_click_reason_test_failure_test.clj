(ns futon3c.diagramprover.wm-wire-flight-record-summary-click-reason-test-failure-test
  "Wire [:flight-record-summary :click-reason-test :failure]: why the
  click closed, as record-summary reads it from the run record's :failure,
  reaching the component's own test
  (futon2/test/futon2/aif/click_reason_test.clj, the :box/kind :test box),
  whose read of this field is

    (is (= (:failure eighth-shaped-record) (:failure e)) \"all four parts\")
    (is (= (:failure e) (flight/click-failure e)))

  in an-eighth-flight-shaped-close-reaches-the-click-entry — e being the
  click entry record-click wrote over record-summary of the run record. A
  test box has no runtime var to drive through, so the hermetic witness
  performs exactly that read over a real call of the writer's var: a judge
  that throws (the box's eighth-flight shape: a substrate-unavailable
  close on a ConnectException) run through
  full-loop-runner/run-opportunity! in hermetic stores, the run record
  read by record-summary, the entry's :failure read beside
  flight/click-failure.

  No live record carries the writer's end: every live run record predates
  WM-CLICK-REASON-I and has no :failure key (see live-records-read, each
  pinned), so the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.learning-trial-ledger :as learning-ledger]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- runner-opts [throw-fn]
  (merge (hermetic/runner-repair-options)
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

(defn- run-record [throw-fn]
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

(defn observe
  "The writer (record-summary) over a real failure's run record, then the
  reader's read of :failure as click_reason_test.clj performs it:
  {:writer the summary's :failure, :reader the entry's :failure,
  :reader-agrees? the box's click-failure comparison}."
  ([] (observe substrate-throw))
  ([throw-fn]
   (let [record (run-record throw-fn)
         summary (fr/record-summary "M-t" "click-1" record)
         e (first (:clicks (flight/record-click
                            (flight/start {:target "M-t" :chosen-because {:kind :requested}}
                                          {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                          {:id "flight-wire"})
                            (merge summary {:wants [:t/b] :before {} :after {}}))))]
     {:writer (:failure summary)
      :reader (:failure e)
      :reader-agrees? (= (:failure e) (flight/click-failure e))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (:path eighth-run-record)
      :sha256 (:sha256 eighth-run-record)
      :why "no :failure key: written before WM-CLICK-REASON-I put the close's failure on the run record"}
     {:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "no :failure key (its one :failure-kind occurrence is a repair finding's, not the run record's)"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the eighth flight's click entry has no :failure — the defect the reader box pins"}]))

(def wire
  {:wire [:flight-record-summary :click-reason-test :failure]
   :kind :witnessed-hermetically
   :test `the-close-failure-reaches-the-components-own-test
   :check check
   :live-records-read live-records-read})

(deftest the-close-failure-reaches-the-components-own-test
  (let [o (check)]
    ;; the reader's own assertions: all four parts, and click-failure agrees
    (is (= {:kind :transport-unavailable :stage :selection
            :error "substrate-2 mission registry unreachable"
            :cause {:cause [{:class "java.net.ConnectException" :message "Connection refused"}]}}
           (:reader o)))
    (is (:reader-agrees? o))
    (is (w/received? o))))

(deftest a-record-with-no-failure-is-a-typed-absence-and-fails-the-wire
  ;; the pinned eighth run record through the same vars: the box's own
  ;; a-record-written-before-this-packet case
  (is (= (:sha256 eighth-run-record) (w/sha256-file (:path eighth-run-record))))
  (let [record (w/read-record (:path eighth-run-record))
        summary (fr/record-summary "M-autoclock-in" "click-1" record)
        e (first (:clicks (flight/record-click
                           (flight/start {:target "M-autoclock-in" :chosen-because {:kind :requested}}
                                         {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                         {:id "flight-wire"})
                           (merge summary {:wants [:t/b] :before {} :after {}}))))]
    (is (= {:absent :failure-not-on-run-record} (:failure e)))
    (is (not (w/received? {:writer (:failure summary) :reader (:failure e)})))))

(deftest a-different-failure-fails-the-wire
  (let [o (check)
        other (observe (fn [] (ex-info "the judge's model returned no parseable decision" {})))]
    (is (some? (:reader other)))
    (is (not (w/typed-absence? (:reader other))))
    (is (not= (:writer o) (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-no-failure
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (is (not (contains? (w/read-record (:path (first live-records-read))) :failure)))
  (is (not (contains? (w/read-record (:path (second live-records-read))) :failure)))
  (is (not-any? :failure (:clicks (:flight (w/read-record (:path (nth live-records-read 2))))))))
