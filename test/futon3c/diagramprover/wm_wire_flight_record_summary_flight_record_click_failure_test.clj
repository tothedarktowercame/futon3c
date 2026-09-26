(ns futon3c.diagramprover.wm-wire-flight-record-summary-flight-record-click-failure-test
  "Wire [:flight-record-summary :flight-record-click :failure]: why the
  click closed, as record-summary reads it from the run record's :failure
  (run-record-failure: the close map's kind/stage/error/cause, each part
  typed absent when the record lacks it, WM-CLICK-REASON-I) reaching the
  flight record's click entry (record-click keeps it: `(contains? click
  :failure) (assoc :failure ...)`).

  No live record carries both ends: every live run record predates
  WM-CLICK-REASON-I and has no :failure key, and every live flight
  record's click entry accordingly carries none (the eighth flight's entry
  said :outcome :incomplete and nothing more — see live-records-read,
  each pinned). So the wire is WITNESSED-HERMETICALLY: a judge that throws
  (a substrate-unavailable close, the click-reason fixture's shape) is run
  through full-loop-runner/run-opportunity! in hermetic stores, and the
  run record it writes — which now carries :failure — is read by
  record-summary (the writer's var) and kept by record-click (the
  reader's var)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.learning-trial-ledger :as learning-ledger]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- runner-opts
  "The r9 wire's isolated options, with a judge that fails selection by
  throwing THROW-FN's exception."
  [throw-fn]
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

(defn check [] (observe substrate-throw))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (:path eighth-run-record)
      :sha256 (:sha256 eighth-run-record)
      :why "no :failure key: written before WM-CLICK-REASON-I put the close's failure on the run record, so the writer's source is absent"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the reader's end absent: the eighth flight's click entry has no :failure — it said :outcome :incomplete and nothing more"}
     {:paths (mapv p ["flight-278b6988.edn" "flight-7f89646a.edn" "flight-e70b4baf.edn"
                      "flight-d00574c8.edn" "flight-ffcd772b.edn" "flight-6cda5ee8.edn"])
      :why "every other flight record: the click entries carry no :failure"}]))

(def wire
  {:wire [:flight-record-summary :flight-record-click :failure]
   :kind :witnessed-hermetically
   :test `the-clicks-failure-reaches-the-click-entry
   :check check
   :live-records-read live-records-read})

(deftest the-clicks-failure-reaches-the-click-entry
  (let [o (check)]
    (is (= {:kind :transport-unavailable :stage :selection
            :error "substrate-2 mission registry unreachable"
            :cause {:cause [{:class "java.net.ConnectException" :message "Connection refused"}]}}
           (:record-failure o))
        "the runner wrote the close's failure onto the run record")
    (is (= (:record-failure o) (:writer o)) "record-summary reads all four parts")
    (is (w/received? o))))

(deftest a-record-with-no-failure-is-a-typed-absence-and-fails-the-wire
  ;; the pinned eighth run record, read through the same vars
  (is (= (:sha256 eighth-run-record) (w/sha256-file (:path eighth-run-record))))
  (let [record (w/read-record (:path eighth-run-record))
        summary (fr/record-summary "M-autoclock-in" "click-1" record)
        e (first (:clicks (flight/record-click
                           (flight/start {:target "M-autoclock-in" :chosen-because {:kind :requested}}
                                         {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                         {:id "flight-wire"})
                           (merge summary {:wants [:t/b] :before {} :after {}}))))
        o {:writer (:failure summary) :reader (:failure e)}]
    (is (not (contains? record :failure)) "the live record predates the field")
    (is (= {:absent :failure-not-on-run-record} (:writer o)))
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-failure-fails-the-wire
  (let [o (check)
        other (observe other-throw)]
    (is (some? (:reader other)))
    (is (not (w/typed-absence? (:reader other))))
    (is (not= (:writer o) (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-no-failure
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path))
  (doseq [path (:paths (nth live-records-read 2))]
    (is (not-any? :failure (:clicks (:flight (w/read-record path)))) path))
  (is (not-any? :failure (:clicks (:flight (w/read-record (:path (second live-records-read))))))))
